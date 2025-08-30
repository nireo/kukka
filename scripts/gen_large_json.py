import json
import random
import argparse

# This script generates a large JSON file to test the performance
# of the kukka implementation of a JSON parser.


def generate_random_data(num_records=1000):
    """Generate a list of random JSON objects"""
    data = []

    for i in range(num_records):
        record = {
            "id": i + 1,
            "name": f"user_{random.randint(1000, 9999)}",
            "age": random.randint(18, 80),
            "score": random.randint(0, 100),
            "level": random.randint(1, 50),
            "points": random.randint(100, 10000),
            "active": random.choice([True, False]),
            "category": random.choice(["A", "B", "C", "D"]),
            "values": [random.randint(1, 1000) for _ in range(random.randint(3, 8))],
        }
        data.append(record)

    return data


def main():
    parser = argparse.ArgumentParser(
        description="Generate a large JSON file for testing."
    )
    parser.add_argument(
        "-n", "--records", type=int, default=10000, help="Number of records to generate"
    )
    args = parser.parse_args()

    num_records = args.records
    print(f"Generating {num_records} records...")

    data = generate_random_data(num_records)

    with open("test_data.json", "w") as f:
        json.dump(data, f, indent=2)

    print(f"Generated {len(data)} records in test_data.json")
    print(f"File size: ~{len(json.dumps(data)) / 1024:.1f} KB")


if __name__ == "__main__":
    main()
