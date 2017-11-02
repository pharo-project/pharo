"
This is a custom help book providing a tutorial for the announcements framework.

"
Class {
	#name : #AnnouncementsTutorial,
	#superclass : #AnnouncementsHelp,
	#category : 'Announcements-Help'
}

{ #category : #accessing }
AnnouncementsTutorial class >> bookName [
	^'Tutorial'
]

{ #category : #accessing }
AnnouncementsTutorial class >> pages [
	^#(step1 step2 step3)
]

{ #category : #pages }
AnnouncementsTutorial class >> step1 [
	"This method was automatically generated. Edit it using:"
	"AnnouncementsTutorial edit: #step1"
	^HelpTopic
		title: 'Step 1 - Define an announcememt'
		contents: 
'To define an announcement you just have to subclass the Announcement class:

   Announcement subclass: #MyInterestingAnnouncement
   	   instanceVariableNames: ''''
	   classVariableNames: ''''
	   poolDictionaries: ''''
	   category: ''MyApp-Core''
	
If required you can add instance variables to hold data that should be transferred when an announcement is made:

   Announcement subclass: #GameLostAnnouncement
	   instanceVariableNames: ''score''
	   classVariableNames: ''''
	   poolDictionaries: ''''
	   category: ''MyGame-Core''!' readStream nextChunkText
]

{ #category : #pages }
AnnouncementsTutorial class >> step2 [
	"This method was automatically generated. Edit it using:"
	"AnnouncementsTutorial edit: #step2"
	^HelpTopic
		title: 'Step 2 - Publishers and subscribers'
		contents: 
'If an object wants to announce an event it needs someone to make the announcement to. This is typically an instance of class Announcer which acts as the mediator between the object that has to announce something (publisher) and one or many (anonymous) subscribers who are interested in the event.

	| announcer |
	announcer := Announcer new.
	announcer announce: MyInterestingAnnouncement new

Using #announce: we can make an announcement - but since nobody is interested yet nothing will happen.

Lets add some consumers/subscribers. Subscribers just register on the Announcer instance to note that they are interested on a particular event/announcement: 

	| announcer |
	announcer := Announcer new.	
	announcer when: MyInterestingAnnouncement send: #open to: Browser.     
	announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.    	

So anytime an interesting announcement is made we want to inform the two consumers with a specific message. Still nothing happens - we have to additionally make the announcement:

	| announcer |
	announcer := Announcer new.	
	announcer when: MyInterestingAnnouncement send: #open to: Browser.    
	announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.    	
	announcer announce: MyInterestingAnnouncement new	

Note that the subscribers are decoupled from the original announcement publisher. They dont have to know each other. Decoupling is the key thing here ... subscribers can register for particular events/announcements and remain anonymous to the original publisher. !' readStream nextChunkText
]

{ #category : #pages }
AnnouncementsTutorial class >> step3 [
	"This method was automatically generated. Edit it using:"
	"AnnouncementsTutorial edit: #step3"
	^HelpTopic
		title: 'Step 3 - More examples'
		contents: 
'In Pharo there is a global called "World" pointing to the desktop morph. This world also has an announcer we can use to demonstrate the features of the framework:

	World announcer 
		when: WindowOpened 
		send: #value 
		to: [ Transcript show: ''A new window was opened'';cr].

So anytime a window is opened in the system a message is shown in the transcript:
		
	Transcript open.
	
	World announcer 
		when: WindowOpened 
		send: #value 
		to: [ Transcript show: ''A new window was opened'';cr].
!' readStream nextChunkText
]
