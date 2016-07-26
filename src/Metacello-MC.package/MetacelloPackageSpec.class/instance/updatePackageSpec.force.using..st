development support
updatePackageSpec: updatedSpecs force: force using: anMCLoader
	"Add pkg copy to updatedSpecs if the file in current image is different from the receiver's file"

	| viName |
	(force not and: [ self getFile == nil ])
		ifTrue: [ ^ self ].	"no file explicitly specified in this spec"
	(viName := self workingCopyNameFor: anMCLoader) == nil
		ifTrue: [ ^ self ].	"no working copy"
	viName ~= self file
		ifTrue: [ 
			| spec |
			spec := self copy.
			spec file: viName.
			updatedSpecs at: spec name put: spec ]
		ifFalse: [ updatedSpecs at: self name put: #uptodate ]