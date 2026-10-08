Extension { #name : 'PragmaSetting' }

{ #category : '*System-Settings-Browser' }
PragmaSetting >> chooseFilePath [
    | result |
    result := self theme chooseFileIn: self currentWorld title: 'Choose a file' extensions: nil path: nil preview: true.
    result ifNotNil: [ self realValue: result fullName.]
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> fontButtonLabel [
	| emphases |
	emphases := IdentityDictionary new at: nil put: 'Regular';
				 at: 0 put: 'Regular';
				 at: 1 put: 'Bold';
				 at: 2 put: 'Italic';
				 at: 4 put: 'Underlined';
				 at: 8 put: 'Narrow';
				 at: 16 put: 'StruckOut';
				 yourself.
	^ StringMorph contents: self realValue familyName , ' '
			, (emphases
					at: self realValue emphasis
					ifAbsent: ['']) , ' ' , self realValue pointSize asString font: self realValue
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> hasEditableList [
	^ false
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> inputMorphFor: aContainer [
	^ self inputWidget
		ifNotNil: [:iw |
			( self theme newRowIn: self for: {iw})
				 clipSubmorphs: true;
				 hResizing: #shrinkWrap;
				 cellInset: 0;
				 yourself]
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> inputWidget [
	"return the default widget for the input a the setting"
	^ self dialog
		ifNotNil: [:d | d numArgs = 0
				ifTrue: [d value]
				ifFalse: [d value: self]]
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> isExportable [

	^ false
]

{ #category : '*System-Settings-Browser' }
PragmaSetting >> labelMorphFor: aContainer [
	^ StringMorph contents: (aContainer model viewedLabelOfSetting: self)
]
