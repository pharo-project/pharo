pages
step2
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