EMACS ?= emacs
LOCAL_PACKAGE_DIR := $(CURDIR)/.packages
PACKAGE_USER_DIR ?= $(LOCAL_PACKAGE_DIR)
PACKAGE_LINT_DIR ?= $(PACKAGE_USER_DIR)/package-lint
PACKAGE_LINT_REV ?= 35996f478d81e51dae4fa30d051f741895d07399
COMPAT_DIR ?= $(PACKAGE_USER_DIR)/compat
COMPAT_REV ?= cccd41f549fa88031a32deb26253b462021d7e12
GHOSTEL_DIR ?= $(PACKAGE_USER_DIR)/ghostel
GHOSTEL_REV ?= 42a5183345fc052b7ce529f4445965b0e145a771
GHOSTEL_VERSION ?= 0.35.0
GHOSTEL_MODULE_VERSION ?= 0.35.1
SHA256 ?= shasum -a 256
HOST_ARCH ?= $(shell uname -m)
HOST_SYSTEM ?= $(shell uname -s)
GHOSTEL_MODULE_ARCH ?= $(if $(filter arm64 aarch64,$(HOST_ARCH)),aarch64,$(HOST_ARCH))
ifeq ($(HOST_SYSTEM),Darwin)
GHOSTEL_MODULE_PLATFORM ?= macos
GHOSTEL_MODULE_SUFFIX ?= dylib
else ifeq ($(HOST_SYSTEM),Linux)
GHOSTEL_MODULE_PLATFORM ?= linux
GHOSTEL_MODULE_SUFFIX ?= so
else ifeq ($(HOST_SYSTEM),FreeBSD)
GHOSTEL_MODULE_PLATFORM ?= freebsd
GHOSTEL_MODULE_SUFFIX ?= so
endif
GHOSTEL_MODULE_FILE = $(GHOSTEL_DIR)/ghostel-module.$(GHOSTEL_MODULE_SUFFIX)
GHOSTEL_MODULE_SIDECAR = $(GHOSTEL_DIR)/ghostel-module.version
GHOSTEL_MODULE_READY = $(GHOSTEL_DIR)/.gve-module-$(GHOSTEL_MODULE_VERSION)-$(GHOSTEL_MODULE_ARCH)-$(GHOSTEL_MODULE_PLATFORM)-$(GHOSTEL_MODULE_SHA256)
GHOSTEL_MODULE_URL = https://github.com/dakra/ghostel/releases/download/v$(GHOSTEL_MODULE_VERSION)/ghostel-module-$(GHOSTEL_MODULE_ARCH)-$(GHOSTEL_MODULE_PLATFORM).$(GHOSTEL_MODULE_SUFFIX)
GHOSTEL_MODULE_VERSION_KEY = $(subst .,_,$(GHOSTEL_MODULE_VERSION))
GHOSTEL_MODULE_SHA256_aarch64_linux_0_35_1 = 272f246383b4b69c6853e21c5f1a856bbc9e2a91e39e9d13f50a7c5c4d1537e5
GHOSTEL_MODULE_SHA256_aarch64_macos_0_35_1 = 1e6042f1b9668b37ce1c7faa3dd85d3cbaf45fbf04e4a892d9b84015d0622dac
GHOSTEL_MODULE_SHA256_x86_64_freebsd_0_35_1 = 4a7b6613c7326906d6613ebf4abb9081df0e63aee0ef6423134ce0a392bb8c7c
GHOSTEL_MODULE_SHA256_x86_64_linux_0_35_1 = a3c1661461a0c527d30a3a0f8871d84bda2eddf25e216d88ec4f6b9ac4afa08f
GHOSTEL_MODULE_SHA256_x86_64_macos_0_35_1 = 9f8f1dd17d0cce29cae4aa7eb6043095c7b8c46d8d402b923f3d0b2983f8a958
GHOSTEL_MODULE_SHA256 ?= $(GHOSTEL_MODULE_SHA256_$(GHOSTEL_MODULE_ARCH)_$(GHOSTEL_MODULE_PLATFORM)_$(GHOSTEL_MODULE_VERSION_KEY))

.PHONY: all bootstrap bootstrap-integration check compile test package-smoke integration integration-strict lint package-lint checkdoc clean distclean

all: check

bootstrap:
	mkdir -p "$(PACKAGE_USER_DIR)"
	if [ ! -d "$(PACKAGE_LINT_DIR)/.git" ]; then \
	  git clone https://github.com/purcell/package-lint.git \
	    "$(PACKAGE_LINT_DIR)"; \
	fi
	git -C "$(PACKAGE_LINT_DIR)" fetch --depth 1 origin \
	  "$(PACKAGE_LINT_REV)"
	git -C "$(PACKAGE_LINT_DIR)" checkout --detach \
	  "$(PACKAGE_LINT_REV)"

bootstrap-integration:
	mkdir -p "$(PACKAGE_USER_DIR)"
	if [ ! -d "$(COMPAT_DIR)/.git" ]; then \
	  git clone --filter=blob:none --no-checkout \
	    https://github.com/emacs-compat/compat.git "$(COMPAT_DIR)"; \
	fi
	git -C "$(COMPAT_DIR)" fetch --depth 1 origin "$(COMPAT_REV)"
	git -C "$(COMPAT_DIR)" checkout --detach "$(COMPAT_REV)"
	if [ ! -d "$(GHOSTEL_DIR)/.git" ]; then \
	  git clone --filter=blob:none --no-checkout \
	    https://github.com/dakra/ghostel.git "$(GHOSTEL_DIR)"; \
	fi
	git -C "$(GHOSTEL_DIR)" fetch --depth 1 origin "$(GHOSTEL_REV)"
	git -C "$(GHOSTEL_DIR)" checkout --detach "$(GHOSTEL_REV)"
	@test -n "$(GHOSTEL_MODULE_SHA256)" || { \
	  echo "No SHA-256 pinned for $(GHOSTEL_MODULE_VERSION) $(GHOSTEL_MODULE_ARCH)-$(GHOSTEL_MODULE_PLATFORM)" >&2; \
	  exit 1; \
	}
	if [ ! -s "$(GHOSTEL_MODULE_FILE)" ] || \
	   [ "$$($(SHA256) "$(GHOSTEL_MODULE_FILE)" 2>/dev/null | awk '{print $$1}')" != \
	     "$(GHOSTEL_MODULE_SHA256)" ] || \
	   [ ! -f "$(GHOSTEL_MODULE_SIDECAR)" ] || \
	   [ "$$(tr -d '\r\n' < "$(GHOSTEL_MODULE_SIDECAR)" 2>/dev/null)" != \
	     "$(GHOSTEL_MODULE_VERSION)" ] || \
	   [ ! -f "$(GHOSTEL_MODULE_READY)" ]; then \
	  $(RM) "$(GHOSTEL_MODULE_READY)"; \
	  curl --fail --location --retry 3 \
	    --output "$(GHOSTEL_MODULE_FILE).tmp" "$(GHOSTEL_MODULE_URL)" && \
	  test "$$($(SHA256) "$(GHOSTEL_MODULE_FILE).tmp" | awk '{print $$1}')" = \
	    "$(GHOSTEL_MODULE_SHA256)" && \
	  mv "$(GHOSTEL_MODULE_FILE).tmp" "$(GHOSTEL_MODULE_FILE)" && \
	  printf '%s\n' "$(GHOSTEL_MODULE_VERSION)" > \
	    "$(GHOSTEL_MODULE_SIDECAR)"; \
	fi
	$(EMACS) -Q --batch -L "$(COMPAT_DIR)" -L "$(GHOSTEL_DIR)/lisp" \
	  -l ghostel \
	  --eval "(unless (featurep 'ghostel-module) (kill-emacs 1))" || \
	  { $(RM) "$(GHOSTEL_MODULE_READY)"; exit 1; }
	touch "$(GHOSTEL_MODULE_READY)"

check: compile test package-smoke lint

compile:
	$(EMACS) -Q --batch -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile ghostel-viewport-editor.el

test:
	$(EMACS) -Q --batch -L . -L test \
	  --eval '(setq load-prefer-newer t)' \
	  -l test/ghostel-viewport-editor-test.el \
	  -f ert-run-tests-batch-and-exit

package-smoke:
	$(EMACS) -Q --batch -L test \
	  -l test/package-smoke.el

integration: bootstrap-integration
	$(EMACS) -Q --batch -L . -L "$(COMPAT_DIR)" -L "$(GHOSTEL_DIR)/lisp" \
	  --eval '(setq load-prefer-newer t)' \
	  -l test/ghostel-viewport-editor-integration-test.el \
	  --eval '(ghostel-viewport-editor-integration-test-run-batch)'

integration-strict: bootstrap-integration
	$(EMACS) -Q --batch -L . -L "$(COMPAT_DIR)" -L "$(GHOSTEL_DIR)/lisp" \
	  --eval '(setq load-prefer-newer t)' \
	  -l test/ghostel-viewport-editor-integration-test.el \
	  --eval '(ghostel-viewport-editor-integration-test-run-batch t)'

lint: package-lint checkdoc

package-lint:
	$(EMACS) -Q --batch -L . -L "$(PACKAGE_LINT_DIR)" \
	  -l test/lint-setup.el \
	  -l package-lint \
	  -f package-lint-batch-and-exit ghostel-viewport-editor.el

checkdoc:
	$(EMACS) -Q --batch -L . -l test/checkdoc-batch.el

clean:
	$(RM) ghostel-viewport-editor.elc

distclean: clean
	$(RM) -r "$(LOCAL_PACKAGE_DIR)"
