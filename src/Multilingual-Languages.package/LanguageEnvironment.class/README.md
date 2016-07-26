The name multilingualized suggests that you can use multiple language at one time.  This is true, of course, but the system still how to manage the primary language; that provides the interpretation of data going out or coming in from outside world. It also provides how to render strings, as there rendering rule could be different in one language to another, even if the code points in a string is the same.

  Originally, LanguageEnvironment and its subclasses only has class side methods.  After merged with Diego's Babel work, it now has instance side methods.  Since this historical reason, the class side and instance side are not related well.

  When we talk about the interface with the outside of the Squeak world, there are three different "channels"; the keyboard input, clipboard output and input, and filename.  On a not-to-uncommon system such as a Unix system localized to Japan, all of these three can have (and does have) different encodings.  So we need to manage them separately.  Note that the encoding in a file can be anything.  While it is nice to provide a suggested guess for this 'default system file content encoding', it is not critical.

  Rendering support is limited basic L-to-R rendering so far.  But you can provide different line-wrap rule, at least.
