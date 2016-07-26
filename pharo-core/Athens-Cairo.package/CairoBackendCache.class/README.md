I providing a simple [weak]key->value storage used to cache various objects used by Athens for Cairo backend, like fonts/forms etc.

The cache is global (there's only a single instance of me used at a time) and
cache is visible globally by all entities of Cairo backend.

The cached objects is held weakly.
The cache is flushed for a new image session.