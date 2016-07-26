I'm the root of Nautilus plugins. You can get the list of Nautilus plugin using the Plugin Manager that you can find in the window menu (right menu on the window itself) of the Nautilus browser window. 

!! How to create Nautilus-Plugins

Here we will give some brief explanations on how to create your own plugin. There are only two requirements
to create a Nautilus-Plugin:

- the class should inherit from ==AbstractNautilusPlugin==
- it should implement the method ==registerTo: aModel==


[[[
AbstractNautilusPlugin
]]]

!!! Announcement subscription

The method ==registerTo:== is used by the plugin to register itself to aModel announcements.

[[[
MyPlugin>>registerTo: aModel

    aModel announcer
   	 when: NautilusKeyPressed 
	 send: #keyPressed: 
	 to: self
]]]

In this example, the instance of ==MyPlugin== subscribes itself to ==NautilusKeyPressed==, and
tell aModel's announcer to send the message  ==keyPressed== to the instance.

So each time a key will be pressed in a Nautilus window the method ==keyPressed:== will be called.

!!! Display
If you want your plugin to add a graphical widget to Nautilus you should override the ==display== method.
This method should return the Morphic element you want Nautilus to display. By default the method returns nil to
notify Nautilus not to display anything.

[[[
MyPlugin>>display
    morph := LabelMorph new contents: 'MyPlugin';
        enabled: false; 
		vResizing: #shrinkWrap; 
		hResizing: #spaceFill; 
		yourself.
	^ morph
]]]

You can also redefine the following methods on the class side:

- ==defaultPosition== defines the default position of the morph. Possible values are =={#top, #middle, #bottom, #none}==. The default value is ==#none==.

- ==possiblePositions== answers a collection of the possible positions the widget could adopt.


!!! Describing your plugin

And finally you can redefine the ==pluginName== method to change the name displayed in the Nautilus Plugin Manager.

[[[
MyPlugin class>>description
	^ 'MyPlugin'
]]]

[[[
MyPlugin class>>description
	^ 'A super cool plugin'
]]]
