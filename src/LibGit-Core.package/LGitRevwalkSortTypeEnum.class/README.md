/**
 * Sort the repository contents in no particular ordering;
 * this sorting is arbitrary, implementation-specific
 * and subject to change at any time.
 * This is the default sorting for new walkers.
 */
#define GIT_SORT_NONE			(0)

/**
 * Sort the repository contents in topological order
 * (parents before children); this sorting mode
 * can be combined with time sorting.
 */
#define GIT_SORT_TOPOLOGICAL (1 << 0)
/**
 * Sort the repository contents by commit time;
 * this sorting mode can be combined with
 * topological sorting.
 */
#define GIT_SORT_TIME			(1 << 1)

/**
 * Iterate through the repository contents in reverse
 * order; this sorting mode can be combined with
 * any of the above.
 */
#define GIT_SORT_REVERSE		(1 << 2)