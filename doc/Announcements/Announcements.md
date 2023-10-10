# Announcements framework

## Introduction

The announcement framework is an event notification framework. Compared to "traditional" event systems in this new framework, an event is a real object rather than a symbol. An event someone might want to announce, such as a button click or an attribute change, is defined as a subclass of the abstract superclass Announcement. 

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

If an object wants to announce an event it needs someone to make the announcement to. This is typically an instance of class `Announcer` which acts as the mediator between the object that has to announce something (publisher) and one or many (subscriber) subscribers who are interested in the event.

```st
| announcer |
announcer := Announcer new.
```

#### Example 1 - Sending a message to the subscriber

For example, if anytime an interesting announcement is made we want to inform two consumers with a specific message. (Still nothing happens - we have to additionally make the announcement later).
Subscribers just register on the Announcer instance to note that they are interested in a particular announcement (event): 

```st
| announcer |
announcer := Announcer new.
announcer when: MyInterestingAnnouncement send: #open to: Browser.
announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.
```

Then using `announce:` we can make an announcement.

```st
| announcer |
announcer := Announcer new.
announcer when: MyInterestingAnnouncement send: #open to: Browser.    
announcer when: MyInterestingAnnouncement send: #inspect to: Smalltalk.
announcer announce: MyInterestingAnnouncement new
```

Note that the subscribers are decoupled from the original announcement publisher. They dont have to know each other. Decoupling is the key thing here ... subscribers can register for particular events/announcements and remain anonymous to the original publisher. 

#### Example 2 - Executing a block closure

There is another way to register subscriber using a block closure as action instead of sending a message to the subscriber.

For example, if we want to show some text in the transcript when any interested announcement is raised:

```st
| announcer |
Transcript open.

announcer := Announcer new.
announcer when: MyInterestingAnnouncement do: [ Transcript show: 'Interesting announcement appeared!' ] for: anObject.
announcer announce: MyInterestingAnnouncement new
```

Note that when the announcement happens just the block closure is evaluated, subscriber is not involved.
Anyway, subscriber object is needed for others aspect of the framework API: unsubscribe, query subscriptors, etc. (See SubscriptionRegistry class for more information)

#### Example 3 - Using global announcer

In Pharo there is a global called `World` pointing to the desktop morph. This world also has an announcer we can use to demonstrate the features of the framework.

In the next example, anytime a window is opened in the system a message is shown in the transcript:

```st
Transcript open.

World announcer 
	when: WindowOpened 
	do: [ Transcript show: 'A new window was opened';cr]
	for: self.
```
