var f = function(a) {
    return function (b) {
        return a + b;
    };
};

var f1 = f(1);
var f2 = f(2);
var x  = f1(2); // should be 3
