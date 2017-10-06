running
setUp

	completeTree := {String . ByteString . Symbol . ByteSymbol . WideSymbol . WideString} .
	partialTree := {String . ByteString . ByteSymbol . WideString}.
	unrelatedClasses :=  {ArrayedCollection . BlockClosure . Class  . Morph . Point}.
	archivesClasses :=  {Archive . TarArchive . ZipArchive . ArchiveMember . TarArchiveMember . ZipArchiveMember . ZipFileMember . ZipDirectoryMember . ZipNewFileMember . ZipStringMember . ZipFileConstants}