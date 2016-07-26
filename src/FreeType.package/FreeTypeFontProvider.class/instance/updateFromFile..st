loading and updating
updateFromFile: aFile
	| i face numFaces cachedInfo info cachedNumFaces path |
	
	i:= 0.
	[(cachedInfo := self validCachedInfoFor: aFile index: i) notNil]
		whileTrue:[
			i = 0 ifTrue: [ cachedNumFaces := cachedInfo numFaces ].
			self addFileInfo: cachedInfo index: i.
			i := i + 1.].
		
	(cachedNumFaces notNil and:[ i >= cachedNumFaces ]) ifTrue:[ ^ self ].
	
	path := aFile fullName convertToWithConverter: UTF8TextConverter new .
	[face := FreeTypeFace basicNew filename: path; index:  i.
	["we use the primNewFaceFromFile:index: method because we want to do this as fast as possible and we don't need the face registered because it will be explicitly destroyed later"
	face primNewFaceFromFile: path index: i.
	face loadFields]
		on: FT2Error
		do: [:e | ^self failedToOpen: face from: path index: i ].
	(face height notNil  and:[face hasFamilyName and:[face hasStyleName and:[face isValid]]])
		ifFalse: [ ^self failedToOpen:face from: path index: i ]
		ifTrue: [
			numFaces ifNil: [numFaces := face numFaces].
			info :=FreeTypeFileInfo new
				absoluteOrRelativePath: aFile path isAbsolute;
				absolutePath: path; "used for quick lookup on same platform"
				locationType: #default;
				index: i;
				fileSize: aFile size;
				modificationTime: aFile modificationTime;
				familyName: face familyName;
				styleName: face styleName;
				postscriptName: face postscriptName;
				bold: face isBold;
				italic: face isItalic;
				fixedWidth: face isFixedWidth;
				numFaces: numFaces;
				extractAttributesFromNames;
				yourself.
			self addFileInfo: info index: i.
			self cacheFileInfo: info index: i.
			"Transcript show: 'from file : ', info asString."
			face destroyHandle].
	i := i + 1.
	i < numFaces "note, we use < rather than <= , because i is zero based"] whileTrue:[].
	
	
		
	
	
	
			
	
	