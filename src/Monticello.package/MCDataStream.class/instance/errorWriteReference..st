other
errorWriteReference: anInteger
    "PRIVATE -- Raise an error because this case of nextPut:'s perform:
     shouldn't be called."

    self error: 'This should never be called'