#### Why does stream implementation seem to leak file descriptors?

```
stack build
stack exec stream-vs-buffer
```

```
# watch our process' file handlers, refreshing every second
lsof -n -r 1 -p "$(pgrep -f stream-vs-buffer)"
```

```
firefox localhost:3000/buffer
# notice that three sqlite file handles are opened: *.sqlite3, *.sqlite3-shm, *.sqlite3-wal
# notice that after several seconds, the file handles are closed
```

```
firefox localhost:3000/stream
# notice that three sqlite file handles are opened: *.sqlite3, *.sqlite3-shm, *.sqlite3-wal
# notice that after waiting many minutes, the file handles are still open
```

_credits: to keep it simple, most of the code is from [Simpler streaming responses](https://www.yesodweb.com/blog/2013/03/simpler-streaming-responses)_
