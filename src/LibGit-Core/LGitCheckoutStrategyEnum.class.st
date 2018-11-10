"
/**
 * Checkout behavior flags
 *
 * In libgit2, checkout is used to update the working directory and index
 * to match a target tree.  Unlike git checkout, it does not move the HEAD
 * commit for you - use `git_repository_set_head` or the like to do that.
 *
 * Checkout looks at (up to) four things: the ""target"" tree you want to
 * check out, the ""baseline"" tree of what was checked out previously, the
 * working directory for actual files, and the index for staged changes.
 *
 * You give checkout one of three strategies for update:
 *
 * - `GIT_CHECKOUT_NONE` is a dry-run strategy that checks for conflicts,
 *   etc., but doesn't make any actual changes.
 *
 * - `GIT_CHECKOUT_FORCE` is at the opposite extreme, taking any action to
 *   make the working directory match the target (including potentially
 *   discarding modified files).
 *
 * - `GIT_CHECKOUT_SAFE` is between these two options, it will only make
 *   modifications that will not lose changes.
 *
 *                         |  target == baseline   |  target != baseline  |
 *    ---------------------|-----------------------|----------------------|
 *     workdir == baseline |       no action       |  create, update, or  |
 *                         |                       |     delete file      |
 *    ---------------------|-----------------------|----------------------|
 *     workdir exists and  |       no action       |   conflict (notify   |
 *       is != baseline    | notify dirty MODIFIED | and cancel checkout) |
 *    ---------------------|-----------------------|----------------------|
 *      workdir missing,   | notify dirty DELETED  |     create file      |
 *      baseline present   |                       |                      |
 *    ---------------------|-----------------------|----------------------|
 *
 * To emulate `git checkout`, use `GIT_CHECKOUT_SAFE` with a checkout
 * notification callback (see below) that displays information about dirty
 * files.  The default behavior will cancel checkout on conflicts.
 *
 * To emulate `git checkout-index`, use `GIT_CHECKOUT_SAFE` with a
 * notification callback that cancels the operation if a dirty-but-existing
 * file is found in the working directory.  This core git command isn't
 * quite ""force"" but is sensitive about some types of changes.
 *
 * To emulate `git checkout -f`, use `GIT_CHECKOUT_FORCE`.
 *
 *
 * There are some additional flags to modified the behavior of checkout:
 *
 * - GIT_CHECKOUT_ALLOW_CONFLICTS makes SAFE mode apply safe file updates
 *   even if there are conflicts (instead of cancelling the checkout).
 *
 * - GIT_CHECKOUT_REMOVE_UNTRACKED means remove untracked files (i.e. not
 *   in target, baseline, or index, and not ignored) from the working dir.
 *
 * - GIT_CHECKOUT_REMOVE_IGNORED means remove ignored files (that are also
 *   untracked) from the working directory as well.
 *
 * - GIT_CHECKOUT_UPDATE_ONLY means to only update the content of files that
 *   already exist.  Files will not be created nor deleted.  This just skips
 *   applying adds, deletes, and typechanges.
 *
 * - GIT_CHECKOUT_DONT_UPDATE_INDEX prevents checkout from writing the
 *   updated files' information to the index.
 *
 * - Normally, checkout will reload the index and git attributes from disk
 *   before any operations.  GIT_CHECKOUT_NO_REFRESH prevents this reload.
 *
 * - Unmerged index entries are conflicts.  GIT_CHECKOUT_SKIP_UNMERGED skips
 *   files with unmerged index entries instead.  GIT_CHECKOUT_USE_OURS and
 *   GIT_CHECKOUT_USE_THEIRS to proceed with the checkout using either the
 *   stage 2 (""ours"") or stage 3 (""theirs"") version of files in the index.
 *
 * - GIT_CHECKOUT_DONT_OVERWRITE_IGNORED prevents ignored files from being
 *   overwritten.  Normally, files that are ignored in the working directory
 *   are not considered ""precious"" and may be overwritten if the checkout
 *   target contains that file.
 *
 * - GIT_CHECKOUT_DONT_REMOVE_EXISTING prevents checkout from removing
 *   files or folders that fold to the same name on case insensitive
 *   filesystems.  This can cause files to retain their existing names
 *   and write through existing symbolic links.
 */
"
Class {
	#name : #LGitCheckoutStrategyEnum,
	#superclass : #LGitExternalEnumerationUInt32,
	#category : #'LibGit-Core-FFI-Enums'
}

{ #category : #'enum description' }
LGitCheckoutStrategyEnum class >> enumDecl [
	^ #(
		GIT_CHECKOUT_NONE  0 "< default is a dry run, no actual updates "

		" Allow safe updates that cannot overwrite uncommitted data "
		GIT_CHECKOUT_SAFE  "1 bitShift: 0" 1

		" Allow all updates to force working directory to look like index "
		GIT_CHECKOUT_FORCE  "1 bitShift: 1" 2


		" Allow checkout to recreate missing files "
		GIT_CHECKOUT_RECREATE_MISSING  "1 bitShift: 2" 4

		" Allow checkout to make safe updates even if conflicts are found "
		GIT_CHECKOUT_ALLOW_CONFLICTS  "1 bitShift: 4" 16

		" Remove untracked files not in index (that are not ignored) "
		GIT_CHECKOUT_REMOVE_UNTRACKED  "1 bitShift: 5" 32

		" Remove ignored files not in index "
		GIT_CHECKOUT_REMOVE_IGNORED  "1 bitShift: 6" 64

		" Only update existing files, don't create new ones "
		GIT_CHECKOUT_UPDATE_ONLY  "1 bitShift: 7" 128

		"
		 * Normally checkout updates index entries as it goes; this stops that.
		 * Implies `GIT_CHECKOUT_DONT_WRITE_INDEX`.
		 "
		GIT_CHECKOUT_DONT_UPDATE_INDEX  "1 bitShift: 8" 256

		" Don't refresh index/config/etc before doing checkout "
		GIT_CHECKOUT_NO_REFRESH  "1 bitShift: 9" 512

		" Allow checkout to skip unmerged files "
		GIT_CHECKOUT_SKIP_UNMERGED  "1 bitShift: 10" 1024
		" For unmerged files, checkout stage 2 from index "
		GIT_CHECKOUT_USE_OURS  "1 bitShift: 11" 2048
		" For unmerged files, checkout stage 3 from index "
		GIT_CHECKOUT_USE_THEIRS  "1 bitShift: 12" 4096

		" Treat pathspec as simple list of exact match file paths "
		GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH  "1 bitShift: 13" 8192

		" Ignore directories in use, they will be left empty "
		GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES  "1 bitShift: 18" 262144

		" Don't overwrite ignored files that exist in the checkout target "
		GIT_CHECKOUT_DONT_OVERWRITE_IGNORED  "1 bitShift: 19" 524288

		" Write normal merge files for conflicts "
		GIT_CHECKOUT_CONFLICT_STYLE_MERGE  "1 bitShift: 20" 1048576

		" Include common ancestor data in diff3 format files for conflicts "
		GIT_CHECKOUT_CONFLICT_STYLE_DIFF3  "1 bitShift: 21" 2097152

		" Don't overwrite existing files or folders "
		GIT_CHECKOUT_DONT_REMOVE_EXISTING  "1 bitShift: 22" 4194304

		" Normally checkout writes the index upon completion; this prevents that. "
		GIT_CHECKOUT_DONT_WRITE_INDEX  "1 bitShift: 23" 8388608

		"
		 * THE FOLLOWING OPTIONS ARE NOT YET IMPLEMENTED
		 "

		" Recursively checkout submodules with same options (NOT IMPLEMENTED) "
		GIT_CHECKOUT_UPDATE_SUBMODULES  "1 bitShift: 16" 65536
		" Recursively checkout submodules if HEAD moved in super repo (NOT IMPLEMENTED) "
		GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED  "1 bitShift: 17" 131072
)
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_allow_conflicts [
	^ self basicNew
		value: 16;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_conflict_style_diff3 [
	^ self basicNew
		value: 2097152;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_conflict_style_merge [
	^ self basicNew
		value: 1048576;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_disable_pathspec_match [
	^ self basicNew
		value: 8192;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_dont_overwrite_ignored [
	^ self basicNew
		value: 524288;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_dont_remove_existing [
	^ self basicNew
		value: 4194304;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_dont_update_index [
	^ self basicNew
		value: 256;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_dont_write_index [
	^ self basicNew
		value: 8388608;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_force [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_no_refresh [
	^ self basicNew
		value: 512;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_none [
	^ self basicNew
		value: 0;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_recreate_missing [
	^ self basicNew
		value: 4;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_remove_ignored [
	^ self basicNew
		value: 64;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_remove_untracked [
	^ self basicNew
		value: 32;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_safe [
	^ self basicNew
		value: 1;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_safe_create [
	^ self basicNew
		value: 2;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_skip_locked_directories [
	^ self basicNew
		value: 262144;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_skip_unmerged [
	^ self basicNew
		value: 1024;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_update_only [
	^ self basicNew
		value: 128;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_update_submodules [
	^ self basicNew
		value: 65536;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_update_submodules_if_changed [
	^ self basicNew
		value: 131072;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_use_ours [
	^ self basicNew
		value: 2048;
		yourself
]

{ #category : #'accessing-values' }
LGitCheckoutStrategyEnum class >> git_checkout_use_theirs [
	^ self basicNew
		value: 4096;
		yourself
]
