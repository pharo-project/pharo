I represent a set of objects without duplicates.  I can hold anything that responds to
#hash and #=, except for nil.  My instances will automatically grow, if necessary,
Note that I rely on #=, not #==.  If you want a set using #==, use IdentitySet.

Instance structure:

  array	An array whose non-nil elements are the elements of the set,
		and whose nil elements are empty slots.  There is always at least one nil.
		In fact I try to keep my "load" at 75% or less so that hashing will work well.

  tally	The number of elements in the set.  The array size is always greater than this.

The core operation is #findElementOrNil:, which either finds the position where an
object is stored in array, if it is present, or finds a suitable position holding nil, if
its argument is not present in array,