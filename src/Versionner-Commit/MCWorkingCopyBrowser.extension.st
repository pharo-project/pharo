Extension { #name : #MCWorkingCopyBrowser }

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> commitNewMajor [
	(VSCommitUnit major: self configurationClass) open
]

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> commitNewMinor [
	(VSCommitUnit minor: self configurationClass) open
]

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> commitNewPatch [
	(VSCommitUnit patch: self configurationClass) open
]

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> configurationClass [
	^ Smalltalk globals at: workingCopy package name asSymbol 
]

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> openWithVersionner [
	VersionnerProjectBrowser openOn: (MBConfigurationInfo  configurationClass: self configurationClass)
]

{ #category : #'*Versionner-Commit-UI' }
MCWorkingCopyBrowser >> versionCommitMenuSpec [
	<contextMenu: #configuration>
	
	^ #(#('Commit new PATCH version' #commitNewPatch) 
		#('Commit new MINOR version' #commitNewMinor) 
		#('Commit new MAJOR version' #commitNewMajor)
		#('Open with Versionner' #openWithVersionner))
]
