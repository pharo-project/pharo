examples
example1: size

   | ts text |

  ts := TextStream on: (Text new: size).
  ts  nextPutAll: 'xxxxx' asText.
  ts nextPutAll: ('yyyyy' asText allBold, 'zzzzzzz' asText).
  text := ts contents.
  ^text
  