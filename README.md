A library (and an accompanying binary) that allows to interactively choose some options.

![](https://dl.dropboxusercontent.com/spa/dlqheu39w0arg9q/qni4-rn_.png)

### Executable

```
Usage: interactive-select [-m|--many]
  Interactively select from whatever is passed to STDIN

Available options:
  -h,--help                Show this help text
  -m,--many                Select multiple options.
```

### Usage

The library exports two functions: `oneOf` and `manyOf`, which accept anything that knows how to draw itself.

```haskell
data Person = Person String Int

instance Option Person where
    showOption (Person name age) = name ++ " (" ++ age ++ " years old)"
    
people = [Person "George" 40, Person "John" 35]    
    
main = manyOf people >>= mapM_ printStrLn
```
