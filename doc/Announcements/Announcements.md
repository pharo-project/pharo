# The Announcement Library

## Introduction

The announcement framework is an event notification framework. Compared to "traditional" event systems in this new framework, an event is a real object rather than a symbol. An event someone might want to announce, such as a button click or an attribute change, is defined as a subclass of the abstract superclass Announcement. 

The subclass can have instance variables for additional information to pass along, such as a timestamp, or mouse coordinates at the time of the event, or the old value of the parameter that has changed. 

To signal the actual occurrence of an event, the "announcer" creates and configures an instance of an appropriate announcement, then broadcasts that instance. Objects subscribed to receive such broadcasts from the announcer receive a broadcast notification together with the instance. They can talk to the instance to find out any additional information about the event that has occurred!


## Announcer

`Announcer` and `Announcement` classes provide an implementation of the [observer design pattern](https://en.wikipedia.org/wiki/Observer_pattern) in Pharo.
As written on the wikipedia page, it aims to address the following problems:

- A one-to-many dependency between objects should be defined without making the objects tightly coupled.
- It should be ensured that, when one object changes state, an open-ended number of dependent objects are updated automatically.
- It should be possible that one object can notify an open-ended number of other objects.

## Getting Started

The `Announcer` class implements a mechanism to:
1. Allow objects (the observers) to subscribe to announcements
2. Manage objects subscriptions
3. Allow the object holding the announcer (the subject) to announce events

For example, there is an announcer available to let objects listen to announcements (events) concerning the system: `SystemAnnouncer uniqueInstance`.
Such announcements include:
- `ClassAdded`
- `ClassRemoved`
- `MethodModified`
- etc.

If one wants to be notified when a class is removed, it is possible to subscribe an object to this specific announcement as follow.

First, let's create the method `#whenClassAdded:` for the object that will listen to `ClassAdded` announcement.
This method will be the hook called when such announcement has been announced.
It takes the announcement sent by the subject as parameter.

```Smalltalk
MyObjectListeningToClassAdded >> whenClassAdded: aClassAdded
    aClassAdded classAffected name traceCr.
    ' has been added.' traceCr
```

Then, we subscribe an instance of our object to the announcer.
When doing that, it is needed to specify
1. Which kind of announcements (its class) our object listen to.
2. Which object listen to this kind of announcements.
3. Which method to call when the annoncement is sent.

The previous step can be achieved as follow:

```Smalltalk
SystemAnnouncer uniqueInstance
  when: ClassAdded send: #whenClassAdded: to: instanceOfMyObjectListeningToClassAdded
```

> Note: the SystemAnnouncer is a bit special because it is a subclass of `Announcer`.
> Usually, when creating you own announcer, you do not subclass `Announcer` but rather use it directly by storing an instance in a dedicated instance variable (see Section "[Using announcer for your subject object](#using-announcer-for-your-subject-object)").

Once the code above has been executed, a message is printed in the Transcript each time a class has been added.
To unsubscribe an object from an announcer, simply call `#unsubscribe:` method on the announcer.

```Smalltalk
SystemAnnouncer uniqueInstance
  unsubscribe: instanceOfMyObjectListeningToClassAdded
```

## Defining new kind of Announcement
Creating a new kind of announcement to fit your needs is easy, just subclass `Announcement` class.
For example, let's say you have an object that has a color. You want to allow observers to listen to color changes of the object.
To do that, let's create a `ColorChangeAnnouncement` class:

```Smalltalk
Announcement << #ColorChangeAnnouncement
	slots: { #newColor } ;
	package: 'MyPackage'
```

Your new kind of announcement aims to hold all the information you need.
In our example, `ColorChangeAnnouncement` hold the new color in `#newColor` instance variable.

## Using Announcer for your Subject Object

The common pattern to use an `Announcer` in your subject object is to:
1. Create an `#announcer` instance variable which will contain the instance of `Announcer`.
2. Create an accessor method for `#announcer` instance variable that use lazy-initialization (so no announcer is created if no announcement is sent and no one wants to subscribe to events of the subject).
3. The subject object send announcements through its announcer.

As an example, let's implement the object changing its color discussed in previous section"

```Smalltalk
Object << #MyObjectChangeColor
	slots: { #announcer. #color }
	package: 'MyPackage'
```

Then we create the accessor for `#announcer` with lazy-initialisation.

```Smalltalk
MyObjectChangeColor >> announcer

  ^ announcer ifNil: [ announcer := Announcer new ]
```

And when the color of the object is updated, we announce it:

```Smalltalk
MyObjectChangeColor >> color: aColor
  color := aColor.
  self announcer
    announce: (ColorChangeAnnouncement new
                newColor: aColor;
                yourself)
```


## Tutorial

### Step 1 - Define an announcememt

To define an announcement you just have to subclass the `Announcement` class:

```st
Announcement << #MyInterestingAnnouncement
	   package: 'MyApp-Core'
```

If required you can add instance variables to hold data that should be transferred when an announcement is made:

```st
Announcement << #GameLostAnnouncement
	   slots: { #score} ;
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
announcer 
	when: MyInterestingAnnouncement 
	do: [ 'Interesting announcement appeared!' traceCr ] 
	for: anObject.
announcer announce: MyInterestingAnnouncement new
```

Note that when the announcement happens just the block closure is evaluated, subscriber is not involved.
Anyway, subscriber object is needed for others aspect of the framework API: unsubscribe, query subscriptors, etc. (See SubscriptionRegistry class for more information)

#### Example 3 - Using global announcer

In Pharo there is a global called `World` pointing to the desktop morph. This world also has an announcer we can use to demonstrate the features of the framework.

In the next example, anytime a window is opened in the system a message is shown in the transcript:

```st
World announcer 
	when: WindowOpened 
	do: [ 'A new window was opened' traceCr ]
	for: self.
```
