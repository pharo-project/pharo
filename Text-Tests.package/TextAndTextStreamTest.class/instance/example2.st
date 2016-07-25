examples
example2

      | ts text |

  ts := TextStream on: (Text new: 50).
  ts  nextPutAll: 'abc' asText.
  ts nextPutAll: 'def' asText allBold.
  ts nextPutAll: 'ghijk' asText.
  text := ts contents.
  ^text
  