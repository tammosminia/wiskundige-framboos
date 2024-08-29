import scala.math.abs

case class User(userId: Int)
case class Parcel(parcelId: Int, userId: Int)

// nParcels = length(parcels)
// nUsers = length(users)
// complexity = O(nParcels * nUsers)
def findParcelsForUsers(parcels: List[Parcel], users: List[User]): List[Parcel] =
  parcels.filter(p => users.contains(p.userId))

// complexity of sort = O(n log n)
// complexity of sorting users = O(nUsers * log nUsers)
// complexity filtering parcels = O(nParcels * log nUsers)
// total = O(nParcels * log nUsers) + O(nUsers * log nUsers)
def findParcelsForUsersWithSort(parcels: List[Parcel], users: List[User]): List[Parcel] = {
  val sortedUsers = users.sortBy(_.userId) // complexity of sort = O(n log n)
  parcels.filter(p => users.contains(p.userId))
}

// complexity of HashSet.contains is O(1) if it used a decent hashing function
// complexity creating a hashed set = O(n)
// complexity of hashing users = O(nUsers)
// complexity of filtering parcels = O(nParcels)
// total = O(nUsers) + O(nParcels)
def findParcelsForUsersWithHash(parcels: List[Parcel], users: List[User]): List[Parcel] = {
  val hashedUsers = users.toSet 
  parcels.filter(p => users.contains(p.userId))
}
