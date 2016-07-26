pages
step1
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