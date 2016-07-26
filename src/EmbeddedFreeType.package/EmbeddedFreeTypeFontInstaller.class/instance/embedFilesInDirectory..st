accessing
embedFilesInDirectory: aDirectory
	"embed all the files in aDirectory
	EmbeddedFreeTypeFontInstaller new 
		provider: FreeTypeFontProvider current;
		embedFilesInDirectory: (FileSystem disk workingDirectory / 'Fonts')
	"
	
	aDirectory files do:[:file |
		file readStreamDo: [ :fileStream|.
			fileStream binary.
			self 
				addFromFileContents: fileStream contents 
				baseName: file basename]].
	
	"update so that missing text styles are created."
	self updateFromSystem.
	
	"clear all the logicalFonts realFonts so that embedded fonts take precedence over external ones"
	LogicalFont allInstances do:[:logFont | logFont clearRealFont]