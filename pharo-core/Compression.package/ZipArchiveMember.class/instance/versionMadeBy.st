accessing - spec fields
versionMadeBy
	"Per .ZIP File Format Specification Version: 6.3.2:
		2 Bytes:
		- The upper byte indicates the compatibility of the file attribute information.  If the external file attributes are compatible with MS-DOS and can be read by PKZIP for DOS version 2.04g then this value will be zero.
		- The lower byte indicates the ZIP specification version (the version of PKWARE Appnote) supported by the software used to encode the file.  The value / 10 indicates the major version number, and the value mod 10 is the minor version number"
	
	^ versionMadeBy.