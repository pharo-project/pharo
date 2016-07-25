pages
page4
	^HelpTopic 
		title: '4. Own help objects'
		contents:	
	'You can open this help browser directly on an instance of HelpTopic, but it is more common to open it on any object that understands the message #asHelpTopic.

So you can write for instance:

   HelpBrowser openOn: Integer

opening a short API help/system reference on the Integer class.
The above expression is the short form for:

   HelpBrowser openOn: (SystemReference forClass: Integer)

If you want you can include the subclasses:

   HelpBrowser openOn: (SystemReference hierarchyFor: Integer)

or even methods

   HelpBrowser openOn: (SystemReference hierarchyWithMethodsFor: Integer)

You can browse the whole system reference documentation using:

    HelpBrowser openOn: SystemReference

But these are only a few examples what we can extract from the 
system. 

However - the major goal is NOT an API browser, the idea is to 
provide a simple architecture to provide browsable help contents 
depending on the context. For instance it should also be possible
to use the help system to provide end user help on any commercial
application that is written with the Smalltalk system.


' 