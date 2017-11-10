Class {
	#name : #MetacelloGoferLoad,
	#superclass : #GoferLoad,
	#category : #Metacello-MC-Gofer
}

{ #category : #private }
MetacelloGoferLoad >> updateCategories [
	MetacelloPlatform current bypassGoferLoadUpdateCategories
		ifFalse: [ super updateCategories ]
]

{ #category : #private }
MetacelloGoferLoad >> updateRepositories [
	"Noop for Metacello...done by loader itself"
]
