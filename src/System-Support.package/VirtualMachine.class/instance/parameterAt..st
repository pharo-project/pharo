parameters
parameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Answer with the current value of that register.
	Fail if parameterIndex has no corresponding register.
	VM parameters are numbered as follows:
	1	end (v3)/size(Spur) of old-space (0-based, read-only)
	2	end (v3)/size(Spur) of young/new-space (read-only)
	3	end (v3)/size(Spur) of heap (read-only)
	4	nil (was allocationCount (read-only))
	5	nil (was allocations between GCs (read-write)
	6	survivor count tenuring threshold (read-write)
	7	full GCs since startup (read-only)
	8	total milliseconds in full GCs since startup (read-only)
	9	incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)
	10	total milliseconds in incremental GCs (SqueakV3) or scavenges (Spur) since startup (read-only)
	11	tenures of surving objects since startup (read-only)
	12-20 were specific to ikp's JITTER VM, now 12-19 are open for use
	20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
	21	root table size (read-only)
	22	root table overflows since startup (read-only)
	23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored
	in image file header).
	24	memory threshold above which shrinking object memory (rw)
	25	memory headroom when growing object memory (rw)
	26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds	(rw) 27	number of times mark loop iterated for current IGC/FGC (read-only)	includes ALL marking
	28	number of times sweep loop iterated for current IGC/FGC (read-only)
	29	number of times make forward loop iterated for current IGC/FGC	(read-only) 30	number of times compact move loop iterated for current	IGC/FGC (read-only)
	31	number of grow memory requests (read-only)
	32	number of shrink memory requests (read-only)
	33	number of root table entries used for current IGC/FGC (read-only)
	34	number of allocations done before current IGC/FGC (read-only)
	35	number of survivor objects after current IGC/FGC (read-only)
	36	millisecond clock when current IGC/FGC completed (read-only)
	37	number of marked objects for Roots of the world, not including Root	Table entries for current IGC/FGC (read-only)
	38	milliseconds taken by current IGC (read-only)
	39	Number of finalization signals for Weak Objects pending when current	IGC/FGC completed (read-only)
	40	BytesPerOop for this image
	41	imageFormatVersion for the VM
	42	number of stack pages in use
	43	desired number of stack pages (stored in image file header, max 65535)
	44	size of eden, in bytes
	45	desired size of eden, in bytes (stored in image file header)
	46	machine code zone size, in bytes (Cog only; otherwise nil)
	47	desired machine code zone size (stored in image file header; Cog only;	otherwise nil)
	48	various header flags. See getCogVMFlags.
	49	max size the image promises to grow the external semaphore table to (0	sets to default, which is 256 as of writing)
	50-51 nil; reserved for VM parameters that persist in the image (such as	eden above)
	52	root table capacity
	53	number of segments (Spur only; otherwise nil)
	54	total size of free old space (Spur only, otherwise nil)
	55	ratio of growth and image size at or above which a GC will be performed	post scavenge
	56	number of process switches since startup (read-only)
	57	number of ioProcessEvents calls since startup (read-only)
	58	number of ForceInterruptCheck calls since startup (read-only)
	59	number of check event calls since startup (read-only)
	60	number of stack page overflows since startup (read-only)
	61	number of stack page divorces since startup (read-only)	62	compiled code compactions since startup (read-only; Cog only; otherwise nil)
	63	total milliseconds in compiled code compactions since startup	(read-only; Cog only; otherwise nil)
	64	the number of methods that currently have jitted machine-code
	65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMTABILITY is bit 1
	66	the byte size of a stack page
	67	the max allowed size of old space (Spur only; nil otherwise; 0 implies	no limit except that of the underlying platform)
	68	the average number of live stack pages when scanned by GC (at	scavenge/gc/become et al)
	69	the maximum number of live stack pages when scanned by GC (at	scavenge/gc/become et al)
	70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)
	71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)"

	<primitive: 254>
	self primitiveFailed