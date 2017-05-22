/**
 * Checkout behavior flags
 *
 * In libgit2, checkout is used to update the working directory and index
 * to match a target tree.  Unlike git checkout, it does not move the HEAD
 * commit for you - use `git_repository_set_head` or the like to do that.
 *
 * Checkout looks at (up to) four things: the "target" tree you want to
 * check out, the "baseline" tree of what was checked out previously, the
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
 * quite "force" but is sensitive about some types of changes.
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
 *   stage 2 ("ours") or stage 3 ("theirs") version of files in the index.
 *
 * - GIT_CHECKOUT_DONT_OVERWRITE_IGNORED prevents ignored files from being
 *   overwritten.  Normally, files that are ignored in the working directory
 *   are not considered "precious" and may be overwritten if the checkout
 *   target contains that file.
 *
 * - GIT_CHECKOUT_DONT_REMOVE_EXISTING prevents checkout from removing
 *   files or folders that fold to the same name on case insensitive
 *   filesystems.  This can cause files to retain their existing names
 *   and write through existing symbolic links.
 */