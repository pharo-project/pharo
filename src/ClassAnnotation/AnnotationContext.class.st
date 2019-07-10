"
My subclasses describe users of annotations.
They should implement single method #describes: with user as argument. User can be any object. It is provided applications during annotation lookup.

By default annotations has NullAnnotationContext which do not describe any possible user. So when you query annotations for given user annotations without specified contexts will be skipped.

For simplicity there is one simple context implementation the SimpleAnnotationContext which represents the hierarchy of user classes. It is hidden from users and created when class is specified for annotation context:
	MyClassAnnotation for: MyUser
Look at subclasses comments for details.
"
Class {
	#name : #AnnotationContext,
	#superclass : #Object,
	#category : #ClassAnnotation
}

{ #category : #converting }
AnnotationContext >> , anAnnotationContext [
	^ CompositeAnnotationContext with: { self. anAnnotationContext asAnnotationContext}
]

{ #category : #converting }
AnnotationContext >> asAnnotationContext [
	^self
]

{ #category : #testing }
AnnotationContext >> describes: anAnnotationUser [
	"The annotation context describes users of annotation.
	Subclasses should decide if given anAnnotationUser is appropriate annotation user.
	anAnnotationUser can be any object"
	self subclassResponsibility 
]
