Important: structs in libgit2 are usually freed by libgit2 itself.
	We therefore disable freeing of structs during GC to prevent invalid
	memory accesses. However, users of any struct need to make sure that
	they free the struct (using a manual #free send) iff it is necessary.
	Otherwise the structs will leak and we can't reclaim the space once the
	object representing the struct has been collected