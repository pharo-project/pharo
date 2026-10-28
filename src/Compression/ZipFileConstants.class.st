"
This class defines magic numbers taken from the PKWARE ZIP Application 
Note [1] approved by ISO/IEC JTC 1/SC 34 N 1621 in July 2011 [2] to be 
the international normative reference for the zip file format.  A class 
imports these bindings as 'class variables' by including the following 
in its class definition: 
    poolDictionaries: 'ZipFileConstants' 
There is a single method on the class side to initialise the values. 

[1] http://www.pkware.com/documents/casestudies/APPNOTE.TXT
[2] http://www.itscj.ipsj.or.jp/sc34/open/1621.pdf

Following are pertinent extracts from the PKWARE ZIP Application Note.... 
All values are stored in little-endian byte order unless otherwise 
specified. 
A. local file header signature     4 bytes  (0x04034b50) 
C. Data descriptor: 
           crc-32                          4 bytes 
           compressed size                 4 bytes 
           uncompressed size               4 bytes 
F. central file header signature   4 bytes  (0x02014b50) 
I. end of central dir signature    4 bytes  (0x06054b50) 
J.  Explanation of fields: 
     * version made by (2 bytes) 
            0 - MS-DOS and OS/2 (FAT / VFAT / FAT32 file systems) 
            3 - UNIX 
            1,2,4-20 not implemented 
     * compression method: (2 bytes) 
            0 - The file is stored (no compression) 
            6 - The file is Imploded 
            8 - The file is Deflated 
            1-7,9-19,97,98 not implemented 
      * For Methods 8 and 9 - Deflating 
            Bit 2  Bit 1 
              0      0    Normal (-en) compression option was used. 
             0      1    Maximum (-exx/-ex) compression option was used. 
              1      0    Fast (-ef) compression option was used. 
             1      1    Super Fast (-es) compression option was 
used.                               
        * internal file attributes: (2 bytes) 
            Bits 1 and 2 are reserved for use by PKWARE. The lowest bit 
of this field indicates, if set, that the file is apparently an ASCII or 
text file.  If not set, that the file apparently contains binary data. 

Note the following do not appear in the PKWARE ZIP Application Note.   
Should they be defined elsewhere? 
""Unix permission bits"" 
DefaultDirectoryPermissions    := 8r040755. 
DefaultFilePermissions        := 8r0100666. 
DirectoryAttrib         := 8r040000. 
FileAttrib             := 8r0100000. 
"
Class {
	#name : 'ZipFileConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'CentralDirectoryFileHeaderSignature',
		'CompressionAES',
		'CompressionDeflated',
		'CompressionLevelDefault',
		'CompressionLevelNone',
		'CompressionStored',
		'DataDescriptorFlag',
		'DataDescriptorLength',
		'DataDescriptorSignature',
		'DefaultDirectoryPermissions',
		'DefaultFilePermissions',
		'DeflatingCompressionFast',
		'DeflatingCompressionMaximum',
		'DeflatingCompressionNormal',
		'DeflatingCompressionSuperFast',
		'DirectoryAttrib',
		'EncryptedFlag',
		'EndOfCentralDirectorySignature',
		'ExtendedTimestampExtraFieldId',
		'FaMsdos',
		'FaUnix',
		'FileAttrib',
		'IfaBinaryFile',
		'IfaTextFile',
		'LanguageEncodingFlag',
		'LocalFileHeaderSignature',
		'StrongEncryptionFlag',
		'SymbolicLinkAttrib',
		'UnicodePathExtraFieldId',
		'UnixFileTypeMask',
		'UnixOwnerExtraFieldId',
		'Zip64EndOfCentralDirectoryLocatorSignature',
		'Zip64EndOfCentralDirectorySignature',
		'Zip64EntriesLimit',
		'Zip64ExtraFieldId',
		'Zip64Limit',
		'Zip64VersionNeededToExtract'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'class initialization' }
ZipFileConstants class >> initialize [

	FaMsdos := 0.
	FaUnix := 3.
	DeflatingCompressionNormal := 0.
	DeflatingCompressionMaximum := 2.
	DeflatingCompressionFast := 4.
	DeflatingCompressionSuperFast := 6.
	CompressionStored := 0.
	CompressionDeflated := 8.
	CompressionLevelNone := 0.
	CompressionLevelDefault := 6.
	IfaTextFile := 1.
	IfaBinaryFile := 0.
	DataDescriptorLength := 12.

	"General purpose bit 11 (Language encoding flag, EFS): the file name and comment are UTF-8"
	LanguageEncodingFlag := 16r800.
	"General purpose bits 0, 3 and 6: encrypted data, data descriptor after the data, PKWARE strong encryption"
	EncryptedFlag := 16r1.
	DataDescriptorFlag := 16r8.
	StrongEncryptionFlag := 16r40.
	"Compression method used by WinZip AES encryption (AE-x)"
	CompressionAES := 99.

	"Zip64 (APPNOTE 4.3.14, 4.3.15 and 4.5.3): sizes, offsets and counts that do not fit are written as Zip64Limit or Zip64EntriesLimit, and the real values are stored in Zip64 records"
	Zip64Limit := 16rFFFFFFFF.
	Zip64EntriesLimit := 16rFFFF.
	Zip64ExtraFieldId := 16r0001.
	Zip64VersionNeededToExtract := 45.
	"0x06064b50 and 0x07064b50 little-endian"
	Zip64EndOfCentralDirectorySignature := #[ 80 75 6 6 ].
	Zip64EndOfCentralDirectoryLocatorSignature := #[ 80 75 6 7 ].

	"Third party extra fields (APPNOTE 4.6)"
	ExtendedTimestampExtraFieldId := 16r5455. "'UT': modification time in seconds since 1970 UTC"
	UnixOwnerExtraFieldId := 16r7875. "'ux': Info-ZIP Unix UID and GID"
	UnicodePathExtraFieldId := 16r7075. "'up': Info-ZIP UTF-8 file name"

	"Unix permission bits"
	DefaultDirectoryPermissions := 8r040755.
	DefaultFilePermissions := 8r0100666.
	DirectoryAttrib := 8r040000.
	FileAttrib := 8r0100000.
	SymbolicLinkAttrib := 8r0120000.
	UnixFileTypeMask := 8r0170000.

	"0x02014b50 little-endian i.e. #(16r50 16r4B 16r01 16r02) [1]"
	CentralDirectoryFileHeaderSignature := #[ 80 75 1 2 ].
	"0x04034b50 little-endian i.e. #(16r50 16r4B 16r03 16r04) [1]"
	LocalFileHeaderSignature := #[ 80 75 3 4 ].
	"0x06054b50 little-endian i.e. #(16r50 16r4B 16r05 16r06) [1]"
	EndOfCentralDirectorySignature := #[ 80 75 5 6 ].
	"0x08074b50 little-endian: the optional signature of a data descriptor (APPNOTE 4.3.9.3)"
	DataDescriptorSignature := #[ 80 75 7 8 ]

	"[1] http://en.wikipedia.org/wiki/Zip_(file_format)#File_headers"
]
