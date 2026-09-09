Extension { #name : 'ActionSettingDeclaration' }

{ #category : '*System-Settings-Browser' }
ActionSettingDeclaration >> inputWidget [
	"Private - Morphic requirement. To be deleted when morphic gets removed"

	^ SimpleButtonMorph new
		target: self target;
		label: (self actionLabel ifNil: [ '(Error: no label defined)' ]);
		actionSelector: self name;
		themeChanged; "This is to avoid green button?"
		yourself
]
