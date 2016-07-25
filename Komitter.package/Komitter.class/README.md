I'm the entry point of this project.
I'm a kind of application model for the Komitter - this is why (it could be discussed) I create the UI of the Kommiter

A classic invocation is

| s k |
s := KomitStagingArea current.
k := Komitter new.
k stagingArea: s.
k open.

k lastCommit  