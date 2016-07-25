This is an abstract class for test cases for the Keymapping project. 

It includes some utility methods to make it simpler to test different key events.

When tests are run this class will ensure that the current Keymap global repository is swiched out with a dummy one to not interfer with the running system.  (see #setUp/#tearDown).


