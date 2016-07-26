I'm an object that saves a buffer of keyevents for the morph I'm attached.
I am the one that dispatches the single and multiple shortcuts.
If the morph has a keymap that matches the keyboard event, I tell the keymap event to execute with the morph I'm attached.