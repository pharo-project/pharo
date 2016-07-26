I am a session handler that will initialize the UIManager during startup.

This session handler makes the assumption that the current UIManager is a startup UI manager when its #startup: method gets called. Then, during startup he will install a Morphic UI manager.

During shutdown we put back a startup ui manager, so we can handle startup actions during next startup without depending in the UI. (However, we shouldnt need a UI manager during the first startup actions).