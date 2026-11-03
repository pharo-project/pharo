"
I am a null find and replace service.

I am used by text areas that do not provide a find and replace service, in place of nil, so that clients never have to check for a missing service. See RubAbstractTextArea class>>defaultFindReplaceServiceClass.

I do nothing: I never open a dialog, I never look for matches, I never replace anything, and I never notify my text area. In particular #findRegex answers nil, which disables the find and replace highlighting of RubExtraSelectionDecorator.
"
Class {
	#name : 'RubNullFindReplaceService',
	#superclass : 'FindReplaceService',
	#category : 'Rubric-Editing-FindReplaceService',
	#package : 'Rubric',
	#tag : 'Editing-FindReplaceService'
}

{ #category : 'services' }
RubNullFindReplaceService >> find [
	^ nil
]

{ #category : 'services' }
RubNullFindReplaceService >> findAndSelect [
	^ nil
]

{ #category : 'services' }
RubNullFindReplaceService >> findAndSelectRegex [
	^ nil
]

{ #category : 'services' }
RubNullFindReplaceService >> findNext [
	^ nil
]

{ #category : 'accessing' }
RubNullFindReplaceService >> findPolicyChanged [
	^ self
]

{ #category : 'services' }
RubNullFindReplaceService >> findRegex [
	"Answer nil so that decorations do not try to highlight matches."
	^ nil
]

{ #category : 'services' }
RubNullFindReplaceService >> replace [
	^ self
]

{ #category : 'services' }
RubNullFindReplaceService >> replaceAll [
	^ self
]

{ #category : 'event handling' }
RubNullFindReplaceService >> whenFindReplaceWindowRequired: anAnnouncement [
	^ self
]

{ #category : 'event handling' }
RubNullFindReplaceService >> whenTextAreaGotFocus: anAnnouncement [
	^ self
]

{ #category : 'event handling' }
RubNullFindReplaceService >> whenTextAreaLostFocus: anAnnouncement [
	^ self
]
