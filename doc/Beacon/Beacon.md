# Beacon: a slim announcement-based object logging engine for Pharo

Logging is an important analysis tool, but most logging systems focus on text. 
There is a historical reason for this, but we can certainly do better. 
Particularly in Pharo, where everything is an object, it should follow that logs should be made of objects and that scrolling endlessly 
through large text files should make room for more sensible querying approaches.

This documentation presents the Beacon subsystem developed originally by T. Girba. 
Beacon represents log objects that are handled by loggers. It is similar to SystemLogger that was an alternate object logging framework that was evaluated for inclusion in Pharo.  
SystemLogger is available at [http://github.com/Ducasse/SystemLogger](http://github.com/Ducasse/SystemLogger). 
The advantage of Beacon is that it is minimalist and reuses the existing announcement infrastructure. 


### Architecture

The architecture of Beacon is illustrated in Figure *@archi@*.



![Beacon architecture. %anchor=archi](figures/beacon.png)

In Beacon, objects emit `BeaconSignal`s (announcement) that are handled by an announcer and loggers that registered to such announcer.
Beacon architectural elements are:
- Domain objects that emit signals using the message `emit`.
- Signal objects that are announcements that management by the Beacon announcing system.
- The class `Beacon` that is just an announcer: an announcement single point managing logger registrations.  The beacon announcer does not do 
anything by itself. From that perspective users are not exposed to it.
- Logger objects are objects that register for local or global sessions of signal reception.


Let us illustrate this in more detail and examples. 

### Basic signals

Beacon works with Announcements. However, for logging purposes, we typically need a timestamp. To this end, `BeaconSignal` offers a handy announcement that stores the current timestamp during the initialization in addition to the process name. 
Right now the display always displays such information and it may be useful in the future to let the developer customize the level of printing. 

The core also defines some concrete signals for convenience:

- `StringSignal` is used for simple string messages. It is the closest to textual way of logging.

- `WrapperSignal` is useful to log an arbitrary object without necessarily creating a new signal class for it. 

- `StackSignal` captures the stack from the calling `StackSignal emit`. 

Note that every application can also define its own signals, in a similar fashion how we do for `Exception`s and `Announcement`s.
For example, we can define a signal that captures the current execution stack or serialize the stack with Fuel (the binary serializer). 


### Basic Loggers
To achieve anything useful, we need a Logger to bind the general Beacon to a concrete medium, such as a stream. 
The core engine comes with a couple of concrete loggers: a memory logger and a textual transcript logger. 
The NewTool package defines another memory logger that is dedicated for a little tool to inspect and navigate logged objects.

Here is a typical scenario we explain after. 

```
l := MemoryLogger new.
l runDuring: [ 'Beacon and Pharo are cool '  emit  ].
l entries
>  an OrderedCollection(2025-04-03T17:24:40.558337+03:00 WrapperSignal 'Beacon and Pharo are cool ')
```

We create a logger, here this is a logger that keeps entries in memory. 
This logger will log emitted objects during the block execution. 

Now let us explore two kind of scenarios: first scope session and after a global session. 

### Local scoped session 

Beacon logger offers the possibility to react to signals within the scope of an execution. The message `runDuring:` is responsible of 
starting and stopping the logger.

```
l := TranscriptLogger new.
l runDuring: [ 'Beacon and Pharo are cool '  emit  ]
```

Here we simply output to the transcript the textual representation of the signals logged within the scope of the block.


This scoping of logging is extremely powerful. It allows one to log only within a given execution. 

### Global session 

Beacon also supports another scenario where a logger will permanently logs any signal. 
The following code snippet illustrates it. 

```
MemoryLogger reset.
MemoryLogger start.
StringSignal emit: 'This is a message' .
MemoryLogger instance entries inspect.
MemoryLogger stop.
```

### Inspecting Beacon objects in the Inspector

Beacon comes with a dedicated set of extensions for the Inspector. In the picture below, you can see an inspector on an instance of RecordingBeacon. The list updates every time a new signal is recorded (See Figure *@inspector@*). 

```
l := MemoryLogger new.
l runDuring: [ 
	'Beacon and Pharo are cool '  emit.
	'Logging objects is really nice' emit ].
l
```

![Inspector. %anchor=inspector](figures/inspector)

You can then define your own signal objects and specific inspector extensions.


### Filtering signal objects

Logging can quickly spawn a ton of data. Filtering is essential in this situation.
You can define dedicated Signals with levels and other information, then filter them in the logger entries using traditional iteration messages such as `select:` or `detect:`..

Now since Beacon is based on Announcements we can also filter emitted signals by limiting our interest to a set of given signal classes. 
For example, the snippet below:

```
(CircularMemoryLogger new
     runFor: StringSignal, WrapperSignal
     during: [
          StringSignal emit: 'This should be recorded.'.
          DummySignal new emit.
          'This should be recorded' emit ]) entries
 ```
          
will only log the two signals (from `StringSignal` and `WrapperSignal` classes), but not the `DummySignal` one. For each application, we can define various combinations of arbitrary signals that should be logged together without needing levels and tags.

Of course, applications can still create their own specialized Logger and/or Signals and introduce another level of filtering if it makes sense within that application. However, please note that the default filtering mechanism offers developers an incentive to design fine-grained logging classes that can be easily picked up at a later time.



### Filtering signal data

Another filtering issue is related to the data associated with a signal object. Let’s take a concrete example. The goal of StackSignal is to store the entire current stack. This in turn can lead to including an extensive object graph that can produce large files. You can for example filter the information by taking only a couple of frame, or converting it into a text.

When you have objects that have many instance variables or when you are more interested the result of methods rather than a complete object
you can define a specific Signal by selecting the corresponding information from the object being logged. 
Here is a sketch

```
BeaconSignal << #ObjectProjectionSignal
	slots: { #object . #slots . #selectors };
	package: 'Example'
```

```
ObjectProjectionSignal >> initialize

	super initialize.
	slots := #().
	selectors := #()
```

```
ObjectProjectionSignal >> asBeaconSignal

	^ ObjectProjectionSignal new 
		object: self;
		slots: #(slot1 slot3);
		selectors: #(methodOne )
		yourself
```

To have a compact form we use STON as follows

```
(STON toString: 
	(#(#x #y) collect: [ :each | each -> (10@20 readSlotNamed: each) ]))
> '[#x:10,#y:20]'
```

And we redefine the method `printOneLineContentsOn:` as follows:

```
ObjectProjectionSignal >> printOneLineContentsOn: stream

	stream << (STON toString: 
		(slots collect: [ :each | each -> (object readSlotNamed: each) ]), 
		(selectors collect: [ :each | each -> (object perform: each) ]))
```

Now we are ready to select information about the logged object.
We could also discard the object once the information is extracted. We could also change the object setter to only extract 
information at the creation of the signal.





