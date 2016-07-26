accessing
addFromFileContents: bytes baseName:  originalFileBaseName
	| i face numFaces info externalMem cachedInfo cachedNumFaces |

	i:= 0.
	[(cachedInfo := self validEmbeddedCachedInfoFor: bytes index: i) notNil]
		whileTrue:[
			i = 0 ifTrue:[cachedNumFaces := cachedInfo numFaces].
			self addFirstFileInfo: cachedInfo index: i.
			i := i + 1.].
	(cachedNumFaces notNil and:[i >= cachedNumFaces]) ifTrue:[^self].
	[externalMem := FreeTypeExternalMemory bytes: bytes.
	externalMem validate.
	face := FreeTypeFace basicNew fileContentsExternalMemory: externalMem .
	[ "we use the primNewFaceFromFile:index: method because we want to do this as fast as possible and we don't need the face registered because it will be explicitly destroyed later"
	face primNewFaceFromExternalMemory: externalMem size: bytes size index: i.
	face loadFields] 
		on: FT2Error 
		do:[:e | 
			self failedToOpen:face index: i.
			^externalMem destroyHandle.].
	(face height notNil  and:[face hasFamilyName and:[face hasStyleName and:[face isValid]]])
		ifFalse:[
			self failedToOpen:face index: i.
			^ externalMem destroyHandle.]
		ifTrue:[ 
			numFaces ifNil: [numFaces := face numFaces].
			info := FreeTypeEmbeddedFileInfo new
				baseName: originalFileBaseName;
				fileContents: bytes;
				index: i;
				familyName: face familyName;
				styleName: face styleName;
				postscriptName: face postscriptName;
				bold: face isBold;
				italic: face isItalic;
				fixedWidth: face isFixedWidth;
				numFaces: numFaces;
				extractAttributesFromNames;
				yourself.
			self addFirstFileInfo: info index: i.
			self cacheEmbeddedFileInfo: info index: i.
			"Transcript show: 'from file : ', info asString."
			face destroyHandle.
			externalMem destroyHandle].
	i := i + 1.
	i < numFaces "note, we use < rather than <= , because i is zero based"] whileTrue:[].