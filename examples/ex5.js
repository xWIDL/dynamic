var f = function (a) {
    return function() {
        return function() {
            return a;
        };
    };
};

var x = f(1)()(); // Should be 1
