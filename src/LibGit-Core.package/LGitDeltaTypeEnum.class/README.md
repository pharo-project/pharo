/**
 * What type of change is described by a git_diff_delta?
 *
 * `GIT_DELTA_RENAMED` and `GIT_DELTA_COPIED` will only show up if you run
 * `git_diff_find_similar()` on the diff object.
 *
 * `GIT_DELTA_TYPECHANGE` only shows up given `GIT_DIFF_INCLUDE_TYPECHANGE`
 * in the option flags (otherwise type changes will be split into ADDED /
 * DELETED pairs).
 */