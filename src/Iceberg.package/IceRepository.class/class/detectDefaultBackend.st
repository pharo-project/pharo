private
detectDefaultBackend
	"Error if not backend"
	TIceRepositoryBackend traitUsers 
		ifEmpty: [ ^ IceError signal: 'There are no configured backend type' ].
	
	"Detect default backend"
	Smalltalk globals 
		at: #IceLibgitLocalRepository
		ifPresent: [ :class | ^ class ].
	
	^ TIceRepositoryBackend traitUsers anyOne