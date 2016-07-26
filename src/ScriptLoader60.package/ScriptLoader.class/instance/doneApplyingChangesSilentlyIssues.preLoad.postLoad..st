ci
doneApplyingChangesSilentlyIssues: issuesToIntegrate preLoad: preload postLoad: postLoad
	| comment commentQuoted st |
	Smalltalk image saveSession.
		"now you can reopen prior to publishing to fix :)"
	self saveAsNewImageWithCurrentReleaseNameToPublish.
	comment := String streamContents: [:stream |
		issuesToIntegrate do: [:each | 
			stream << each name << 'Thanks to ' << each content author email ; lf;lf.
			stream << each url;lf;lf;lf.]].
	comment := comment trimBoth.
	
	"comment := UIManager default
		multiLineRequest: 'Comment for this update.'
		initialAnswer: self commentForCurrentUpdate
		answerHeight: 200.
	comment ifNil: [^ self]."
	
	commentQuoted := comment copyReplaceAll: (String with: $') with: (String with: $' with: $').
	st := String new writeStream.
	st nextPutAll: 'commentForCurrentUpdate'; cr.
	st nextPutAll: ' ^ '.
	st nextPut: $'.
	st nextPutAll: commentQuoted.
	st nextPut: $'.
	self class
		compile: (st contents)
		classified: 'public'.

	self class waitingCacheFolder deleteAllChildren.
	self saveChangedPackagesIntoWaitingFolder.
	self 
		generateScriptAndUpdateMethodForNewVersionSilentlyConfigurations: #() 
		preLoad: preload 
		postLoad: postLoad.
	self saveLatestScriptLoaderToWaitingFolder.