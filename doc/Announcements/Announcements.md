# Announcements framework


## Introduction
The announcement framwork is an event notification framework. Compared to "traditional" event systems in this new framework, an event is a real object rather than a symbol. An event someone might want to announce, such as a button click or an attribute change, is defined as a subclass of the abstract superclass Announcement. 

The subclass can have instance variables for additional information to pass along, such as a timestamp, or mouse coordinates at the time of the event, or the old value of the parameter that has changed. 

To signal the actual occurrence of an event, the "announcer" creates and configures an instance of an appropriate announcement, then broadcasts that instance. Objects subscribed to receive such broadcasts from the announcer receive a broadcast notification together with the instance. They can talk to the instance to find out any additional information about the event that has occurred!


## Tutorial
### Step 1 - Define an announcememt

To define an announcement you just have to subclass the `Announcement` class:

```st
Announcement subclass: #MyInterestingAnnouncement
   	   instanceVariableNames: ''
	   classVariableNames: ''
	   package: 'MyApp-Core'
```	

If required you can add instance variables to hold data that should be transferred when an announcement is made:

```st
Announcement subclass: #GameLostAnnouncement
	   instanceVariableNames: 'score'
	   classVariableNames: ''
	   package: 'MyGame-Core'
```
	   
### Step 2 - Publishers and subscribers

If an object wants to announce an event it needs someone to make the announcement to. This is typically an instance of class `Announcer` which acts as the mediator between the object that has to announce something (publisher) and one or many (anonymous) subscribers who are interested in the event.

```st
| announcer |
announcer := Announcer new.
announcer announce: MyInterestingAnnouncement new
```

Using `announce:` we can make an announcement - but since nobody is interested yet nothing will happen.

Lets add some consumers/subscribers. Subscribers just register on the Announcer instance to note that they are interested on a particular event/announcement: 

```st
| announcer |
announcer := Announcer new.	
announcer when: MyInterestingAnnouncement send: #open to: Browser.     
announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.    	
```

So anytime an interesting announcement is made we want to inform the two consumers with a specific message. Still nothing happens - we have to additionally make the announcement:

```st
| announcer |
announcer := Announcer new.	
announcer when: MyInterestingAnnouncement send: #open to: Browser.    
announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.    	
announcer announce: MyInterestingAnnouncement new
```

Note that the subscribers are decoupled from the original announcement publisher. They dont have to know each other. Decoupling is the key thing here ... subscribers can register for particular events/announcements and remain anonymous to the original publisher. 

### Step 3 - More examples

In Pharo there is a global called `World` pointing to the desktop morph. This world also has an announcer we can use to demonstrate the features of the framework:

```st
World announcer 
	when: WindowOpened 
	send: #value 
	to: [ Transcript show: 'A new window was opened';cr].
```

So anytime a window is opened in the system a message is shown in the transcript:

```st
Transcript open.
	
World announcer 
	when: WindowOpened 
	send: #value 
	to: [ Transcript show: 'A new window was opened';cr].
```

