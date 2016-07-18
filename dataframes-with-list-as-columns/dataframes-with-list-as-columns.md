# Populating data frame cells with more than one value
Mark Heckmann  




## Data frames are lists

Most R users will know that data frames are lists. You can easily verify that a data frame is a list by typing


```r
d <- data.frame(id=1:2, name=c("Jon", "Mark"))
d
```

```
  id name
1  1  Jon
2  2 Mark
```

```r
is.list(d)
```

```
[1] TRUE
```

However, data frames are lists with some special properties. For example, all entries in the list must have the same length (here 2), etc. You can find a nice description of the differences between lists and data frames [here](http://stackoverflow.com/questions/15901224/what-is-difference-between-dataframe-and-list-in-r). Accessing the first column of `d`, we find that it contains a vector (and a factor in case of column `name`). Note, that `[[ ]]` is an operator to select a list element. As data frames are lists, they will work here as well.


```r
is.vector(d[[1]])
```

```
[1] TRUE
```


## Data frame columns may contain lists

A long time, I was unaware of the fact, that data frames may also contain lists as columns instead of vectors. For example, let's assume Jon's children are *Mary* and *James*, and Mark's children are called *Greta* and *Sally*. Their names are stored in a list with two elements. We can add them to the data frame like this:


```r
d$children <- list(c("Mary", "James"), c("Greta", "Sally"))
d
```

```
  id name     children
1  1  Jon  Mary, James
2  2 Mark Greta, Sally
```

A single data frame entry in column `children` now contains more than one value. Given that the column is a list, not a vector,  we cannot go as usual when modifying an entry of the column. For example, to change Jon's children, we cannot do


```r
> d[1 , "children"] <- c("Mary", "James", "Thomas")

Error in `[<-.data.frame`(`*tmp*`, 1, "children", value = c("Mary", "James",  : 
  replacement has 3 rows, data has 1
```

Taking into account the list structure of the column, we can type the following to change the values in a single cell.


```r
d[1 , "children"][[1]] <- list(c("Mary", "James", "Thomas"))

# or also

d$children[1] <- list(c("Mary", "James", "Thomas"))
d
```

```
  id name            children
1  1  Jon Mary, James, Thomas
2  2 Mark        Greta, Sally
```

You can also create a data frame having a list as a column using the `data.frame` function, but with a little tweak. The list column has to be wrapped inside the function `I`. This will protect it from several conversions taking place in `data.frame` (see `?I` documentation).


```r
d <- data.frame(id = 1:2, 
                name = c("Jon", "Mark"),
                children = I(list(c("Mary", "James"), 
                                  c("Greta", "Sally")))
                )
```

This is an interesting feature, which gives me a deeper understanding of what a data frame is. But when exactly would I want to use it? I have not encountered the need to use it very often yet (though of course there may be plenty of situations where it makes sense). But today I had a case where this feature seemed particularly useful.


## Converting lists and data frames to JSON

I had two separate types of information. One stored in a data frame and the other one in a list Referring to the example above, I had


```r
d <- data.frame(id=1:2, name=c("Jon", "Mark"))
d
```

```
  id name
1  1  Jon
2  2 Mark
```

and 


```r
ch <- list(c("Mary", "James"), c("Greta", "Sally"))
ch
```

```
[[1]]
[1] "Mary"  "James"

[[2]]
[1] "Greta" "Sally"
```

I needed to return an array of JSON objects which look like this.

```
[
  {
    "id": 1,
    "name": "Jon",
    "children": ["Mary", "James"]
  },
  {
    "id": 2,
    "name": "Mark",
    "children": ["Greta", "Sally"]
  }
] 
```

Working with the superb `jsonlite` package to convert R to JSON, I could do the following to get the result above.


```r
library(jsonlite)

l <- split(d, seq(nrow(d)))             # convert data frame rows to list
l <- unname(l)                          # remove list names
for (i in seq_along(l))                 # add element from ch to list
  l[[i]] <- c(l[[i]], children=ch[i])

toJSON(l, pretty=T, auto_unbox = T)     # convert to JSON
```

The results are correct, but getting there involved quite a number of tedious steps. These can be avoided by directly placing the list into a column of the data frame. Then `jsonlite::toJSON` takes care of the rest.


```r
d$children <- list(c("Mary", "James"), c("Greta", "Sally"))
toJSON(d, pretty=T, auto_unbox = T)
```

```
[
  {
    "id": 1,
    "name": "Jon",
    "children": ["Mary", "James"]
  },
  {
    "id": 2,
    "name": "Mark",
    "children": ["Greta", "Sally"]
  }
] 
```

Nice :) What we do here, is basically creating the same nested list structure as above, only now it is disguised as a data frame. However, this approach is much more convenient.



