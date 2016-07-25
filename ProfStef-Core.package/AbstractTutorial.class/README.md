Parent class of all Pharo tutorials.

To create your own tutorial:
- subclass AbstractTutorial
- implement a few methods which returns a Lesson instance
- implement tutorial which returns a Collection of selectors to the methods you've created.

For example, see MockTutorial (minimalist) and PharoySntaxTutorial (default PharoTutorial one).

See the PharoTutorial class comment to execute your own tutorial.