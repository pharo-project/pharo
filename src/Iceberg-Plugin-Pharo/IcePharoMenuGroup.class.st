Class {
	#name : #IcePharoMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Iceberg-Plugin-Pharo-Core'
}

{ #category : #accessing }
IcePharoMenuGroup >> icon [
	^ (self iconNamed: #pharo) scaledToSize: 16 @ 16
]

{ #category : #accessing }
IcePharoMenuGroup >> name [

	^ 'Pharo'
]

{ #category : #accessing }
IcePharoMenuGroup >> order [
	"Plugins are SmallInteger maxVal - 1999 .. SmallInteger maxVal"
	^ SmallInteger maxVal - 1999
]
