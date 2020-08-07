"
I'm a holder of some samples for Pillar structures.

"
Class {
	#name : #PRDocumentSample,
	#superclass : #PRDocument,
	#category : #'Pillar-ExporterRichText'
}

{ #category : #'annotated paragraph' }
PRDocumentSample >> annotatedParagraph [
	"PRPillarParserMain parse: '@@note one more annotated!'"
	
	^ self documentWith: (PRAnnotatedParagraph new
					annotation: 'note';
					add: (PRText content: 'one more annotated!'); 
					 yourself)
	
]

{ #category : #formats }
PRDocumentSample >> bold [
	^ self withFormat: PRBoldFormat
]

{ #category : #'annotated paragraph' }
PRDocumentSample >> codeblock [
	"PRPillarParserMain parse: '[[[
1 + 2 
	* 3  
]]]
'"
	
	^ self documentWith:  (PRCodeblock new
					text:  '1 + 2 
	* 3  '; 
					 yourself)
]

{ #category : #helpers }
PRDocumentSample >> documentWith: anElement [

	^ PRDocument new
		add: anElement; 
		yourself
]

{ #category : #headers }
PRDocumentSample >> headerLevel1 [
	^ self headerWithLevel: 1
]

{ #category : #headers }
PRDocumentSample >> headerLevel2 [
	^ self headerWithLevel: 2
]

{ #category : #headers }
PRDocumentSample >> headerLevel3 [
	  ^ self headerWithLevel: 3
]

{ #category : #headers }
PRDocumentSample >> headerLevel4 [
	  ^ self headerWithLevel: 4
]

{ #category : #headers }
PRDocumentSample >> headerLevel5 [
	^ self headerWithLevel: 5
]

{ #category : #headers }
PRDocumentSample >> headerLevel6 [
	^ self headerWithLevel: 6 
]

{ #category : #headers }
PRDocumentSample >> headerWithLevel: aLevel [
	  ^ self withStructure: (PRHeader new level: aLevel; yourself)
]

{ #category : #formats }
PRDocumentSample >> italic [
	^ self withFormat: PRItalicFormat 
]

{ #category : #formats }
PRDocumentSample >> monospace [
	^ self withFormat: PRMonospaceFormat 
]

{ #category : #formats }
PRDocumentSample >> monospacePoint [
	^ self with: 'Point' format: PRMonospaceFormat 
]

{ #category : #list }
PRDocumentSample >> nestedList [
	^ PRDocument new
		add: (PROrderedList new
			add: (PRListItem new 
				add: (PRText content: 'item n1'); yourself);
			add: (PRUnorderedList new
				add: (PRListItem new 
					add: (PRText content: 'item a'); yourself);
				add: (PRListItem new 
					add: (PRText content: 'item b'); yourself);
				yourself);
			add: (PRListItem new 
				add: (PRText content: 'item n2'); 
				yourself);
		yourself); 
	yourself.
]

{ #category : #list }
PRDocumentSample >> orderedList [
	^ PRDocument new
		add: (PROrderedList new
			add: (PRListItem new 
				add: (PRText content: 'item n1'); yourself);
			add: (PRListItem new 
				add: (PRText content: 'item n2'); yourself);
		yourself); yourself.
]

{ #category : #preformatted }
PRDocumentSample >> preformatted [
	^ PRDocument new
		add: (PRPreformatted new
					add: (PRText content: 'foo'); 
					add: (PRText content: 'barwith a space') ;yourself);
		yourself
]

{ #category : #list }
PRDocumentSample >> simpleOrderedList [

	^ (PROrderedList
			withAll:
				{(PRListItem with: (PRText content: 'First')).
				(PRListItem with: (PRText content: 'Second'))})
]

{ #category : #list }
PRDocumentSample >> simpleUnorderedList [

	^ (PRUnorderedList
			withAll:
				{(PRListItem with: (PRText content: 'First')).
				(PRListItem with: (PRText content: 'Second'))})
]

{ #category : #list }
PRDocumentSample >> unorderedList [
	^ PRDocument new
		add: (PRUnorderedList new
			add: (PRListItem new 
				add: (PRText content: 'item a'); yourself);
			add: (PRListItem new 
				add: (PRText content: 'item b'); yourself);
		yourself); yourself.
]

{ #category : #helpers }
PRDocumentSample >> with: aString format: aPRFormatClass [
	^ (aPRFormatClass with: (PRText content: aString))
               
]

{ #category : #helpers }
PRDocumentSample >> withFormat: aPRFormatClass [
	^ self documentWith: 
            (PRParagraph new
                add: (aPRFormatClass with: (PRText content: 'Foo'));
                yourself)
			
]

{ #category : #helpers }
PRDocumentSample >> withStructure: aPRObject [
	^ self documentWith: (aPRObject add: (PRText content: 'foo'); yourself)
]
