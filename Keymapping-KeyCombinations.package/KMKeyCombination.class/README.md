I'm an abstract class representing a key combination. I'm can represent several cases of key presses:

- simple key presses: pressing a single key, as a letter or number, or others like tab or space
- modified key presses: a simple key + a modifier like shift or alt
- option key presses: a list of key presses where only one of them should be valid
- chained shortcuts: a sequence of shortcuts

My more important protocols are:

- matching: contains methods to control if a key combination is equals to other or matches a sequence of keyboard events
- combining: defines simple ways to combine shorcut objects, like chaining them or modifying them

For more information, look at my subclasses.