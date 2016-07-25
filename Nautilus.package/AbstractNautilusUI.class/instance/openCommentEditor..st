icons behavior
openCommentEditor: aClass
	| newComment |

	newComment := Smalltalk ui theme
		textEditorIn: window
		text: 'Enter a comment for the class ', aClass theNonMetaClass name,':'
		title: 'Comment Editor'
		entryText: aClass theNonMetaClass comment
		entryHeight: 300.
	
	newComment
		ifNotNil: [ aClass theNonMetaClass comment: newComment asString ]