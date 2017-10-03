private
httpGet: url username: username pass: pass do: aBlock

  | client |
  client := WebClient new.
  username ifNotNil: [client username: username; password: [pass]].
  ^ (client httpGet: url do: aBlock)
    content;
    close