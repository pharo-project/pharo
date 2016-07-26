Subinstances of me are members in a ZipArchive.
They represent different data sources:
	* ZipDirectoryMember -- a directory to be added to a zip file
	* ZipFileMember -- a file or directory that is already in a zip file
	* ZipNewFilemember -- a file that is to be added to a zip file
	* ZipStringMember -- a string that is to be added to a zip file

They can write their data to another stream either copying, compressing,
or decompressing as desired.