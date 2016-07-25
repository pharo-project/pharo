I am a singleton object, subscribed to system events, to listen to the creation of methods marked with the <keymap> and keymap:> pragmas.

When I listen one of those events, I reinitialize the KMRepository default instance and reload it with all declared keymaps.