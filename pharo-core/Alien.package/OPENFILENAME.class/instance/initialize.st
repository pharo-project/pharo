instance initalization
initialize
	self
		unsignedLongAt: self lStructSizeOffset put: self class dataSize;
		unsignedLongAt: self lpstrFileOffset put: (self class Ccalloc: self MAXPATH);
		unsignedLongAt: self nMaxFileOffset put: self MAXPATH