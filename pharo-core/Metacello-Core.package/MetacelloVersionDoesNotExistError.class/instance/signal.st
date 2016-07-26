Signaling
signal
  "GemStone needs this to print exception correctly"

  self messageText: self description.
  ^ super signal