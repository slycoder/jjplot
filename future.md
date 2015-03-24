# Features for future releases #


## Ordering Stats ##
Here we propose a few ways that we might allow for the ordering of stats.  This feature is not present in ggplot2 but would be quite convenient to have.  Example applications would be generating facets of ethnicity breakdowns sorted by frequency.

> ### A simple solution ###

> Adding a special sort statistic.   In this framework, you could do:

> `jjplot( ~ bar() : sort(by = y) : table() + x)`

> In effect, the sort statistic would be an identity statistic but it would add a special attr to the data frame.   This is convenient because right now scales are constructed when the first geom is encountered --- the input data frame is used to create the scale.  This routine would just have to look for this attr and do the sort then.


## Named Stats ##
This idea was originally conceived so that one could refer to the results of a stat or stack of stats in options or special functions that are not the geom that the stats are bound to.  The original motivation is to provide a way to sort by stats, but Jon's solution for sorting is quite elegant and I think we should go with that.  Another reason I had proposed stat names is that it makes it easier to add support for things like pre-compiled stats.
> ### Syntax ###
> Adding named statistics and an additional jjplot parameter.  In this framework, you could do:
> > `jjplot( ~ bar(width = 0.5) : .ethnicCount(table()) + x,, data = df)`

> or
> > `jjplot( ~ bar(width = 0.5) : table(name='ethnicCount') + x, data = df)`


> In the first proposed syntax, we wrap a stat (or stacked stats) in a function name.  In Eytan's opinion, this improves readability.  In the second syntax, we specify a name parameter to the stat we would like to name.  Since stats are applied from left to right, the rightmost name would refer to the top of the stats stack, so that `jitter():table(name='ethnicCount')` is equivalent to `ethnicCount(jitter():table())`.

> ### Pre-compiled Stats ###
> Pre-compiling stats to save on time when you are trying to tweak the aesthetics of a plot involving a computationally expensive stat.  It might work something like this:
```
  dc = jjcompile(y, ethnicCount(table()), data = df)
  jjplot( ~ bar(width = 0.5) : ethnicCount + x, data = dc)
```

> Of course, table() is quite cheap, but if we add support for clustering, 2d kernel density estimates, etc, this might come in handy.

---

> > I like the idea of having a special geom called 'save' which would dump it into a structure future jjplots can use.  It might be also awesome to have an xtable geom.