"
A CairoFontFaceExecutor is reponsible of releasing the external 'cairo_font_face_t *' data structure when a corresponding CairoFontFace instance in Smalltalk object memory has been garbage collected.

The external data structure may still be referenced from other cairo objects in the external heap as explained in cairo online documentation https://www.cairographics.org/manual/cairo-FreeType-Fonts.html .

It is thus vital that we do not release the underlying FreeType face until the number of references drops to 1, meaning that we are the last reference, and that we can safely really finalize the external resource.

In order to prevent the free type face to be garbage collected (and its associated FT_face resource to be released), we use an instance variable just for retaining a string reference to it.

More details on the object graph can be found on Pharo issue tracker at
https://pharo.fogbugz.com/f/cases/20776/Potential-dangling-ref-to-freed-FT_Face

"
Class {
	#name : #CairoFontFaceExecutor,
	#superclass : #FFIExternalResourceExecutor,
	#instVars : [
		'ftFace'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #finalization }
CairoFontFaceExecutor >> finalize [
	session = Smalltalk session ifFalse: [ ^ self ].
	(resourceClass hasSharedResourceData: data) ifFalse: [ ^self reallyFinalize].

	UIManager default inform: 'Attempt to release a shared cairo_font_face.
Please report this on pharo-dev mailing list'.

	[ "Regularly poll for the right moment to release the resource"
	[20 seconds wait.
	session = Smalltalk session and: [ resourceClass hasSharedResourceData: data ] ] whileTrue.
	self reallyFinalize] forkAt: Processor userBackgroundPriority
]

{ #category : #accessing }
CairoFontFaceExecutor >> ftFace: aFace [
	"Retain a strong reference to the FreeType face, until we really release the external cairo_font_face resource."
	ftFace := aFace
]

{ #category : #finalization }
CairoFontFaceExecutor >> reallyFinalize [
	ftFace := nil.
	session = Smalltalk session ifFalse: [ ^ self ].
	resourceClass finalizeResourceData: data
]
