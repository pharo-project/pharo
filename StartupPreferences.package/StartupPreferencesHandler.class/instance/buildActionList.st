actions
buildActionList
	"All found files will be executed."
	
	| directories |
	directories := OrderedCollection new.
	directories add: [ self lookInGeneralPreferencesFolder ].
	directories add: [ self lookInPreferencesVersionFolder ].
	directories add: [ self lookInImageFolder ].
	^ directories