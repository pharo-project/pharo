private events
mouseTrailFrom: currentBuf 
  "Current event, a mouse event buffer, is about to be processed.  If there are other similar mouse events queued up, then drop them from the queue, and report the positions inbetween."

  | nextEvent trail |
  trail := (Array new: 1) writeStream.
  trail nextPut: currentBuf third @ currentBuf fourth.
  [(nextEvent := Sensor peekEvent) isNil] whileFalse: 
      [nextEvent first = currentBuf first 
        ifFalse: [^trail contents "different event type"].
      nextEvent fifth = currentBuf fifth 
        ifFalse: [^trail contents "buttons changed"].
      nextEvent sixth = currentBuf sixth 
        ifFalse: [^trail contents "modifiers changed"].
      "nextEvent is similar.  Remove it from the queue, and check the next."
      nextEvent := Sensor nextEvent.
      nextEvent 
        ifNotNil: [ trail nextPut: nextEvent third @ nextEvent fourth ]].
  ^trail contents