"
I am used to export the classic help stored as special methods into microdown files.

"
Class {
	#name : #HelpExporter,
	#superclass : #Object,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #'as yet unclassified' }
HelpExporter class >> export [
	<script>
	self new writeAllTopics 
]

{ #category : #'as yet unclassified' }
HelpExporter >> demoteContents: contents toLevel: level [
	^ ((contents copyReplaceAll: (String cr,'### ') with: (String cr, ('#' repeat: 3+level),' '))
		copyReplaceAll: (String cr,'## ') with: (String cr, ('#' repeat: 2+level),' '))
		copyReplaceAll: (String cr,'# ') with: (String cr, ('#' repeat: 1+level),' ')
]

{ #category : #'as yet unclassified' }
HelpExporter >> rootDirectory [
	"return a path to the root directory to store microdown files"
	| root |
	root := 'doc' asFileReference.
	root ensureCreateDirectory.
	^ root
]

{ #category : #'as yet unclassified' }
HelpExporter >> systemHelpTopics [
	^ SystemHelp asHelpTopic 
]

{ #category : #'as yet unclassified' }
HelpExporter >> writeAllTopics [
	|topics|
	CustomHelp formatter: CustomHelpMicrodownFormatter new.
	topics := self systemHelpTopics.
	topics subtopics do: [ :topic | self writeTopic: topic ].
	CustomHelp formatter: nil "force default"
]

{ #category : #writing }
HelpExporter >> writeStreamFromTopic: topic [
	| fileName fileRef aStream |
	fileName := 'gen_', (topic title asLowercase select: [ :c | c isAlphaNumeric  ]).
	fileRef := (self rootDirectory  / fileName, 'md' ) asFileReference.
	fileRef deleteIfAbsent: [ "ignore" ].
	aStream :=  fileRef writeStream .
	^ aStream 
]

{ #category : #'as yet unclassified' }
HelpExporter >> writeTopic: topic [
	| writeStream |
	writeStream := (self writeStreamFromTopic: topic).
	self writeTopic: topic level: 1 toStream: writeStream.
	writeStream flush;close
]

{ #category : #'as yet unclassified' }
HelpExporter >> writeTopic: topic level: level toStream: stream [ 
	stream  << ('#' repeat: level) << ' ' << topic title << String cr.
	topic contents ifNotEmpty: 
		[ stream << (self demoteContents: topic contents  toLevel: level) << String cr. ].
	topic subtopics do: [ :t | self writeTopic: t level: level +1 toStream: stream ]
	
]
