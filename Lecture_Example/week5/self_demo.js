class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    size() {
        return Math.sqrt(this.x * this.x + this.y * this.y);
    }

    sameSize(other) {
        return this.size() === other.size();
    }
}
