I am a refactoring splitting a cascade message send to multiple messages.

You can select an interval containing a cascade expression. The refactoring will split this expression to two message sends to the receiver. 

My preconditions verify that the selector containing the cascaded message send is defined in this class, and a cascade message can be found.

If the receiver of the cascade expression is a literal or the return value of another message send, I will add another temporary variable for the interim result.