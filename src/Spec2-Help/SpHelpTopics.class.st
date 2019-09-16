"
A SpecHelpTopics is the main class which holds the documentation of Spec. This documentation can be accessed from the Help Browser.
See class side for selectors.

"
Class {
	#name : #SpHelpTopics,
	#superclass : #CustomHelp,
	#category : #'Spec2-Help-Topics'
}

{ #category : #accessing }
SpHelpTopics class >> bookName [
	^ 'Spec'
]

{ #category : #pages }
SpHelpTopics class >> introduction [
	^HelpTopic 	
		title: 'About Spec'
		contents: 'Spec has been originally developed by B. Van Ryseghem based on an idea of S. Ducasse. It is now maintained by the community and the pharo core team.. 

Spec is an attempt to support UI logic reuse. Spec is influenced by VisualWorks and Dolphin MVP in the sense that it recognizes the need for a Presenter or Application-Model class, in Spec called SpPresenter that manages the logic and the link between widgets and domain objects. Spec, following the tradition, uses value holders, simple object raising announcements when their values, often domain objects, change.'
]

{ #category : #accessing }
SpHelpTopics class >> key [
	^'Spec'
]

{ #category : #accessing }
SpHelpTopics class >> pages [
	^#(introduction terminology #SpExamples #SpTheHeartOfSpec #SpLayouts)
]

{ #category : #pages }
SpHelpTopics class >> terminology [
	^HelpTopic
		title: 'Terminology'
		contents: 'To avoid possible misunderstandings due to confusion in terminology, we first define the following four terms, which will be used frequently:

UI Element: an interactive graphical element displayed as part of the graphical user interface.

UI Model: an object that contains the state and behavior of one or several UI elements.

Widget: the union of a UI Element and its UI model.

Basic Widgets: low level widgets like'
		
]
