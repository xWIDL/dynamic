var f = function () /* @f */ {
    // ctx [@f]
    var x = { a : 1 }; // on-heap allocation

    // Upon returning the closure, we should do reachability analysis to only
    // returning the reachable store. Any *free* name in closure must refer to
    // some name in closing scope, we shall keep going up until we find some
    // Value. And we will do reachability analysis recursively on this value
    return /* @cs */ function() /* @g */  {
        // This closure should capture the defining ctx [@f] at callsite @cs
        // The [x -> ref *a] and [*a -> { a : 1 }] will be propagated to @cs

        // ctx [@g, @f]
        return x;
    };
};

// ctx []

var x = f()(); // { a : 1 }
